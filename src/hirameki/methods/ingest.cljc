(ns hirameki.methods.ingest
  "hirameki 閃き — G8/G9 LIVE primary-source ingest (USPTO Open Data Portal).

  Makes the live authoritative ingest EXECUTABLE. The key-free PatentsView bulk was retired
  into the USPTO Open Data Portal (data.uspto.gov / api.uspto.gov), which needs a FREE API
  key. Split, kaname-style (ADR-2606172100), into:

    • a PURE normalizer  `odp->patent` / `odp->patents` / `field-rows` / `merge-corpus`
      — real ODP patent JSON (already parsed to Clojure data) → hirameki `:patent` rows with
      `:sourcing :authoritative` + a CITED `:source` URL. Fully unit-tested against a fixture;
      no network, no key. This is the part whose correctness we can prove offline.

    • a LIVE fetch leg  `fetch-odp!` / `ingest!`
      — reads the operator's key from env `USPTO_ODP_API_KEY` (NO-SERVER-KEY: the key is the
      operator's, never held/committed by the platform), GETs api.uspto.gov, then folds the
      authoritative rows into the corpus and re-materializes the DataLad snapshot (dataset.cljc).

  HARD INVARIANTS (proven by test):
    G3 disclosed-only — title / assignee / status / CPC are mirrored ODP facts, never re-judged
    G6 NO person-level inventor — the assignee is the ORG; inventor names are DROPPED at ingest
    G2 a patent is the gated object — no :imposes edge is ever produced
    G8 no-server-key — the API key comes from env, is never written to disk/corpus/provenance
    G9 authoritative rows carry a cited :source URL (the ODP record), `:sourcing :authoritative`
  ADR-2606212200.

  ⚠ SCHEMA NOTE: `odp->patent` maps the documented USPTO ODP `patentFileWrapperDataBag`
  shape. The exact JSON field names must be VERIFIED against the live response on the first
  keyed run (the fixture encodes the documented shape); `ingest!` prints a sample so a
  mismatch is caught immediately rather than silently mis-mapped."
  (:require [clojure.string :as str]
            [hirameki.methods.analyze :as a]
            [hirameki.methods.normalize :as nz]
            [hirameki.methods.dataset :as ds]
            #?(:clj [hirameki.methods.hirameki-edn :as he])
            [json.compat :as json]
            #?(:clj [clojure.java.io :as io]))
  #?(:clj (:import (java.net URI URLEncoder)
                   (java.net.http HttpClient HttpRequest HttpResponse$BodyHandlers)
                   (java.nio.charset StandardCharsets)
                   (java.time Duration))))

(def odp-base "https://api.uspto.gov/api/v1/patent/applications/search")
(def odp-record-url "https://data.uspto.gov/patent/application")

;; ── pure helpers ─────────────────────────────────────────────────────────────
;; Re-exported from `hirameki.methods.normalize`, which is where they now live so
;; that `corpus.cljc` and `dataset.cljc` can share them without a require cycle.
;; The names stay put: they are this namespace's published surface.
(def assignee->keyword nz/assignee->keyword)
(def cpc->section nz/cpc->section)
(def cpc->subclass nz/cpc->subclass)
(def year-of nz/year-of)
(def merge-corpus nz/merge-corpus)

(defn- odp-status->status [s]
  (let [d (some-> s str/lower-case)]
    (cond
      (nil? d) :pending
      (re-find #"patent(ed)?|granted|issue" d) :granted
      (re-find #"abandon" d) :lapsed
      (re-find #"expire" d) :expired
      :else :pending)))

(defn odp->patent
  "PURE: one ODP patentFileWrapper record (parsed JSON, Clojure data) → a hirameki :patent row.
  Reads bibliographic facts only; DROPS inventor names (G6). assignee = the org applicant.
  Carries :sourcing :authoritative + a cited :source URL. nil if there is no usable id."
  [rec]
  (let [meta (or (get rec "applicationMetaData") (get rec :applicationMetaData) {})
        gv  (fn [m k] (or (get m k) (get m (keyword k))))
        app-num (or (gv rec "applicationNumberText") (gv meta "applicationNumberText"))
        pat-num (gv meta "patentNumber")
        id (some-> (or pat-num app-num) str)
        title (gv meta "inventionTitle")
        cpc (or (some-> (gv meta "cpcClassificationBag") first str)
                (some-> (gv meta "cpcClassificationBag") (get 0) str))
        ;; assignee/applicant ORG only — inventorBag intentionally NOT read (G6)
        applicant (or (some-> (gv meta "applicantBag") first (gv "applicantNameText"))
                      (some-> (gv meta "firstApplicantName"))
                      (gv meta "assigneeEntityName"))
        filing (year-of (gv meta "filingDate"))
        grant  (year-of (gv meta "grantDate"))
        status (odp-status->status (gv meta "applicationStatusDescriptionText"))]
    (when id
      (cond-> {:type :patent
               :id id
               :title (or title "(untitled)")
               :jurisdiction :us
               :field (or (cpc->subclass cpc) "UNKNOWN")
               :assignee (or (assignee->keyword applicant) :unassigned)
               :filing-year filing
               :term-years 20
               :status (if (and pat-num (= status :pending)) :granted status)
               :open-license :none
               :sourcing :authoritative
               :source (str odp-record-url "/" (or pat-num app-num))}
        grant (assoc :grant-year grant)))))

(defn odp->patents
  "PURE: a bag of ODP records → distinct hirameki :patent rows (drops un-id'd)."
  [recs]
  (->> recs (keep odp->patent) (distinct) vec))

;; ── live fetch leg (:clj, no-server-key — key from env) ──────────────────────
#?(:clj
   (defn api-key []
     (let [k (System/getenv "USPTO_ODP_API_KEY")]
       (when (str/blank? k)
         (throw (ex-info "USPTO_ODP_API_KEY not set — get a free key at https://data.uspto.gov (no-server-key: the key is the operator's, read from env, never committed)" {})))
       k)))

#?(:clj
   (def ^:private ^HttpClient http-client
     (delay (-> (HttpClient/newBuilder)
                (.connectTimeout (Duration/ofSeconds 20))
                (.build)))))

#?(:clj
   (defn- url-encode [s]
     (URLEncoder/encode (str s) StandardCharsets/UTF_8)))

#?(:clj
   (defn fetch-odp!
     "LIVE: GET the USPTO ODP patent search API. opts: :q (query string), :rows (default 25).
     Returns the parsed `patentFileWrapperDataBag` vector. Read-only; key from env.

     Uses java.net.http (JDK 11+) rather than babashka.http-client — bb was retired
     as a runtime here (ADR-2607173000) and this leg needs no dependency at all."
     [{:keys [q rows] :or {rows 25}}]
     (let [uri (URI/create (str odp-base
                                "?q=" (url-encode (or q "applicationMetaData.inventionTitle:*"))
                                "&rows=" (url-encode (str rows))))
           req (-> (HttpRequest/newBuilder uri)
                   (.header "X-API-KEY" (api-key))
                   (.header "Accept" "application/json")
                   (.timeout (Duration/ofSeconds 60))
                   (.GET)
                   (.build))
           resp (.send @http-client req (HttpResponse$BodyHandlers/ofString))
           status (.statusCode resp)
           body-str (.body resp)]
       (when (>= status 400)
         (throw (ex-info (str "ODP fetch failed http=" status)
                         {:status status
                          :body (subs (str body-str) 0 (min 400 (count (str body-str))))})))
       (let [body (json/parse-string body-str)]
         (or (get body "patentFileWrapperDataBag") (get body "results") [])))))

#?(:clj
   (defn ingest!
     "LIVE G8/G9: fetch authoritative patents → fold into the existing seed corpus →
     re-materialize the DataLad snapshot (corpus + datoms + manifest). Returns a summary.
     `as-of` is metadata only (not content-addressed). No-server-key."
     [{:keys [q rows seed-path data-dir as-of]
       :or {seed-path "kotoba/seed.edn"
            as-of "manual"}}]
     (let [data-dir (or data-dir (System/getenv "HIRAMEKI_DATASET_REPO"))
           _ (when-not (seq data-dir)
               (throw (ex-info "HIRAMEKI_DATASET_REPO not set and :data-dir not supplied" {})))
           recs (fetch-odp! {:q q :rows rows})
           _ (when (seq recs)
               (println "sample ODP record keys:" (sort (keys (first recs)))))
           fetched (odp->patents recs)
           existing (he/patents seed-path)
           merged (merge-corpus existing fetched)
           man (ds/write! merged data-dir as-of)]
       {:fetched (count fetched)
        :existing (count existing)
        :authoritative (count (filter #(= :authoritative (:sourcing %)) merged))
        :total (count merged)
        :corpus-cid (get-in man [:artifacts :corpus :cid])})))

#?(:clj
   (defn -main [& args]
     (let [q (or (first args) "applicationMetaData.inventionTitle:semiconductor")
           rows (parse-long (or (second args) "25"))
           as-of (or (nth args 2 nil) "manual")]
       (try
         (let [r (ingest! {:q q :rows rows :as-of as-of})]
           (println (str "ingested: fetched=" (:fetched r) " existing=" (:existing r)
                         " → total=" (:total r) " (authoritative=" (:authoritative r) ")"))
           (println (str "corpus cid=" (:corpus-cid r))))
         (catch clojure.lang.ExceptionInfo e
           (println "INGEST BLOCKED:" (.getMessage e))
           (println "This is the operator G8/G9 step. Set USPTO_ODP_API_KEY and re-run:")
           (println "  HIRAMEKI_DATASET_REPO=… USPTO_ODP_API_KEY=… bb src/hirameki/methods/ingest.cljc \"<query>\" 50")
           (System/exit 2))))))
