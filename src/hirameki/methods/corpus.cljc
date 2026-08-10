(ns hirameki.methods.corpus
  "hirameki 閃き — the harvested Google Patents journal → hirameki patent rows.

  The journal (`cloud-itonami/hirameki-patents/80-data/public/*.journal.edn`) is
  a flat vector of `[entity attr value tx op]` quads written by the harvest leg.
  This namespace folds it back into the `:patent` row shape the rest of the
  actor already speaks, and computes the aggregates the corpus can actually
  support.

  ## What the corpus supports, and what it does not

  A Google Patents page carries bibliographic metadata but **no CPC
  classification**. So this corpus supports:

    ✅ the RELEASE CLOCK per patent (filing year + term → years-to-expiry)
    ✅ ASSIGNEE concentration over the corpus as a whole
    ✅ jurisdiction and grant-year distribution

  and it does NOT support:

    ❌ per-CPC-field concentration — `analyze/analyze-field` needs a
       `:cpc-section`, and inventing one from the title would be fabrication

  Field-level analytics stay with the curated `kotoba/seed.edn` (which carries
  real CPC sections) and with the USPTO ODP ingest (`ingest.cljc`, which returns
  `cpcClassificationBag`). Rows produced here are marked `:field \"UNKNOWN\"`
  rather than guessed, and `analyze` simply has no field rows to work with —
  the honest outcome, not a silent zero.

  ## G6 is enforced here, at the boundary

  The journal DOES carry `:patent/inventor` — the harvester stores what the page
  published. This namespace **drops it**. A hirameki row never carries a natural
  person; the assignee is the ORG. That is gate G6, and `corpus-test` asserts a
  journal containing inventors produces rows containing none."
  (:require [clojure.string :as str]
            [hirameki.methods.analyze :as a]
            [hirameki.methods.normalize :as nz]))

;; ── quads → entity maps ──────────────────────────────────────────────────────

(defn quads->entities
  "Fold `[e a v tx op]` quads into `{entity {attr value-or-vector}}`.

  An attr seen once holds its value; an attr seen more than once (the
  cardinality-many case, e.g. `:patent/cites`) holds a vector in first-seen
  order. `:retract` quads remove. Deterministic: no map ordering escapes,
  because callers sort by id downstream."
  [quads]
  (reduce
   (fn [acc [e attr v _tx op]]
     (if (= :retract op)
       (update acc e dissoc attr)
       (update-in acc [e attr]
                  (fn [cur]
                    (cond
                      (nil? cur) v
                      (vector? cur) (if (some #{v} cur) cur (conj cur v))
                      (= cur v) cur
                      :else [cur v])))))
   {}
   quads))

(defn- as-vec [x] (cond (nil? x) [] (vector? x) x :else [x]))

(defn- jurisdiction [country]
  (when (and country (not (str/blank? country)))
    (keyword (str/lower-case country))))

;; ── entity map → hirameki :patent row ────────────────────────────────────────

(defn- sorted-distinct
  "Multi-valued attribute → a canonical vector.

  Folding quads into a map preserves FIRST-SEEN order, which is quad order —
  so `(first applicants)` would silently depend on how the journal happened to
  be laid out, and a re-fold of the same facts in a different order would name a
  different assignee. Sorting makes the fold a function of the facts alone.
  (`corpus-test` shuffles the journal and asserts the rows are identical.)"
  [x]
  (->> (as-vec x) (remove nil?) distinct sort vec))

(defn entity->patent
  "One journal entity → a hirameki `:patent` row, or nil when there is no usable id.

  `:sourcing :authoritative` — these are mirrored bibliographic facts with a
  cited `:source` URL, never re-judged (G3). Inventors are DROPPED (G6).

  Co-assignment is real and common (US8697359B1 is held by the Broad Institute
  AND MIT). All co-assignees are kept in `:assignees`; `:assignee` is the first
  of them in canonical order, because the field analytics downstream take a
  single holder per patent. The plural is where the truth is; the singular is a
  projection of it."
  [m]
  (let [id (or (:patent/patent-id m) (:patent/number m))
        filed (nz/year-of (:patent/filed-at m))
        granted (nz/year-of (:patent/granted-at m))
        assignees (->> (sorted-distinct (:patent/applicant m))
                       (keep nz/assignee->keyword)
                       distinct
                       vec)]
    (when (and id (not (str/blank? (str id))))
      (cond-> {:type :patent
               :id (str id)
               :title (or (some-> (:patent/title m) str/trim) "(untitled)")
               :jurisdiction (or (jurisdiction (:patent/country m)) :unknown)
               :field "UNKNOWN"            ; no CPC on a Google Patents page — not guessed
               :assignee (or (first assignees) :unassigned)
               :term-years 20
               :status (if granted :granted :pending)
               :open-license :none
               :sourcing :authoritative
               :source (:patent/source-url m)}
        (seq assignees) (assoc :assignees assignees)
        filed (assoc :filing-year filed)
        granted (assoc :grant-year granted)
        (seq (as-vec (:patent/cites m)))
        (assoc :cites (sorted-distinct (:patent/cites m)))))))

(defn journal->patents
  "Journal quads → distinct `:patent` rows sorted by id. Deterministic."
  [quads]
  (->> (quads->entities quads)
       vals
       (keep entity->patent)
       (sort-by :id)
       vec))

;; ── corpus-level aggregates (what this corpus CAN answer) ───────────────────

(defn assignee-concentration
  "Top assignees by patent count over the whole corpus, plus a named-HHI.

  This is the corpus analogue of `analyze/named-hhi`: a LOWER BOUND on
  concentration, because `:unassigned` (the fragmented residual) is excluded
  exactly the way `:other` is excluded there. `top-n` defaults to 10."
  ([rows] (assignee-concentration rows 10))
  ([rows top-n]
   (let [named (remove #(= :unassigned (:assignee %)) rows)
         total (count named)
         freqs (frequencies (map :assignee named))
         shares (when (pos? total)
                  (into {} (map (fn [[k v]] [k (* 100.0 (/ v (double total)))])) freqs))]
     {"corpus_size" (count rows)
      "named" total
      "unassigned" (- (count rows) total)
      "distinct_assignees" (count freqs)
      "top" (->> freqs
                 (sort-by (juxt (comp - val) (comp str key)))
                 (take top-n)
                 (mapv (fn [[k v]] {"assignee" (str k)
                                    "count" v
                                    "share" (when shares (a/round3 (get shares k)))})))
      "named_hhi" (if (pos? total)
                    (a/round3 (/ (reduce + 0.0 (map #(* % %) (vals shares))) 10000.0))
                    0.0)})))

(defn release-distribution
  "Counts by release-status over the corpus — the aggregate release clock."
  ([rows] (release-distribution rows a/default-ref-year))
  ([rows ref-year]
   (->> rows
        (map #(a/release-status % ref-year))
        frequencies
        (into (sorted-map-by (fn [x y] (compare (str x) (str y)))))
        (map (fn [[k v]] [(name k) v]))
        (into {}))))

(defn jurisdiction-distribution [rows]
  (->> rows
       (map :jurisdiction)
       frequencies
       (sort-by (juxt (comp - val) (comp str key)))
       (mapv (fn [[k v]] {"jurisdiction" (name k) "count" v}))))

(defn citation-stats
  "Citation-graph shape. `known` is how many cited ids are already harvested —
  the honest measure of how closed the graph is, and the size of the frontier
  the harvest loop still has to walk."
  [rows]
  (let [ids (into #{} (map :id) rows)
        cited (into [] (mapcat #(get % :cites [])) rows)
        distinct-cited (into #{} cited)]
    {"edges" (count cited)
     "distinct_cited" (count distinct-cited)
     "cited_already_harvested" (count (filter ids distinct-cited))
     "frontier" (count (remove ids distinct-cited))
     "mean_out_degree" (if (seq rows)
                         (a/round3 (/ (count cited) (double (count rows))))
                         0.0)}))

(defn summarize
  "Everything the harvested corpus can honestly say about itself."
  ([rows] (summarize rows a/default-ref-year))
  ([rows ref-year]
   {"ref_year" ref-year
    "assignees" (assignee-concentration rows)
    "release" (release-distribution rows ref-year)
    "jurisdictions" (jurisdiction-distribution rows)
    "citations" (citation-stats rows)}))

;; ── datom emission ───────────────────────────────────────────────────────────

(defn summary-datoms
  "Corpus-level observation datoms.

  Emits NO `:hirameki/infringement-verdict`, `:hirameki/fto-opinion` or
  `:hirameki/equity-signal` (G1/G3), and NO `:hirameki.patent/imposes-on` (G2).
  A concentration reading is a disclosed-fact aggregate, never a verdict about
  any holder."
  [{:strs [ref_year assignees release jurisdictions citations]}]
  (let [e "hirameki-corpus:google-patents"]
    (into
     [(a/add e ":hirameki.corpus/ref-year" ref_year)
      (a/add e ":hirameki.corpus/size" (get assignees "corpus_size"))
      (a/add e ":hirameki.corpus/named" (get assignees "named"))
      (a/add e ":hirameki.corpus/distinct-assignees" (get assignees "distinct_assignees"))
      (a/add e ":hirameki.obs/named-hhi" (get assignees "named_hhi"))
      (a/add e ":hirameki.corpus/citation-edges" (get citations "edges"))
      (a/add e ":hirameki.corpus/citation-frontier" (get citations "frontier"))
      (a/add e ":hirameki.corpus/mean-out-degree" (get citations "mean_out_degree"))
      (a/add e ":hirameki/sourcing" ":authoritative")
      (a/add e ":hirameki/derived" true)]
     (concat
      (map (fn [[status n]]
             (a/add (str "hirameki-release:" status) ":hirameki.release/count" n))
           (sort release))
      (map (fn [{:strs [jurisdiction count]}]
             (a/add (str "hirameki-jurisdiction:" jurisdiction)
                    ":hirameki.jurisdiction/count" count))
           jurisdictions)
      (map (fn [{:strs [assignee count]}]
             (a/add (str "hirameki-assignee:" assignee)
                    ":hirameki.assignee/patent-count" count))
           (get assignees "top"))))))
