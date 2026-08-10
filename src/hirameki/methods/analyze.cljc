(ns hirameki.methods.analyze
  "hirameki 閃き — public-patent KG-mirror analytics (edge-primary, on read).

  The all-technology generalization of tokigusuri 時薬 (the pharma patent-cliff subset):
  per TECHNOLOGY FIELD it computes exclusivity-CONCENTRATION (top-assignee share +
  named-HHI, the :other long-tail excluded → a lower bound) and a RELEASE-READINESS
  buffer (expired + expiring-soon + open-licensed shares), then routes each field to
  RELEASE (解放) / open-license / de-monopolization / monitor. Per PATENT it reads the
  RELEASE CLOCK (years-to-expiry → release-status). NO score is stored; everything is
  computed on read from the kotoba seed.

  HARD INVARIANTS (proven by test):
    G1  release map, NEVER an infringement / FTO / patentability verdict or equity signal
    G2  a patent is the GATED OBJECT, never a 取-holder — only an assignee/holder imposes
    G3  patent status + assignee share are DISCLOSED facts, never re-judged, never a forecast
  ADR-2606212200 (supersedes 2604251024)."
  (:require [clojure.string :as str]
            [clojure.set :as set]
            #?(:clj [hirameki.methods.hirameki-edn :as he])))

(def default-ref-year 2026)

;; ── helpers ──────────────────────────────────────────────────────────────────
(defn round3
  "Round to 3 decimals. Public because the corpus aggregates (corpus.cljc) must
  round exactly the way the field aggregates here do — two roundings that drift
  produce two different numbers for the same quantity."
  [x]
  (/ (Math/round (* (double x) 1000.0)) 1000.0))
(defn- clamp01 [x] (max 0.0 (min 1.0 (double x))))
(defn add [entity attr value] [":db/add" entity attr value])

(defn- named-assignees
  "Assignee pairs excluding the fragmented :other residual."
  [f]
  (remove (fn [[a _]] (= a :other)) (:assignees f)))

;; ── field analytics ────────────────────────────────────────────────────────
(defn top-assignee [f]
  (let [as (named-assignees f)]
    (when (seq as) (first (apply max-key second as)))))

(defn top-assignee-share
  "Largest single named-assignee share (percent). 0 if none named."
  [f]
  (let [as (named-assignees f)]
    (if (seq as) (apply max (map second as)) 0)))

(defn named-hhi
  "Herfindahl–Hirschman index over NAMED assignee shares, normalized to 0..1.
  A lower bound on concentration (the :other long tail is fragmented, excluded)."
  [f]
  (let [shares (map second (named-assignees f))]
    (/ (reduce + 0 (map #(* % %) shares)) 10000.0)))

(defn chokepoint-risk
  "Exclusivity chokepoint level from the top assignee share."
  [top-share]
  (cond
    (>= top-share 60) :critical
    (>= top-share 40) :high
    (>= top-share 25) :moderate
    :else :low))

(defn release-readiness
  "Fraction of the field already in, or heading to, the commons ∈ 0..1:
  expired + half the expiring-soon + 0.4·open-licensed. A buffer that DISCHARGES
  exclusivity load (the 解放 the patent system promises after the statutory term)."
  [f]
  (clamp01 (+ (double (or (:expired-share f) 0))
              (* 0.5 (double (or (:expiring-soon-share f) 0)))
              (* 0.4 (double (or (:open-license-share f) 0))))))

(defn exclusivity-load
  "Edge-primary exclusivity load ∈ 0..1: top-assignee concentration weighted by the
  field's DISCLOSED standard-essentiality (an essential field's concentration bites
  harder). Computed on read; never stored."
  [f]
  (let [ess (double (or (:essentiality f) 0))]
    (* (/ (top-assignee-share f) 100.0) (+ 0.5 (* 0.5 ess)))))

(defn route
  "Where the observation is routed — exclusivity → commons. NEVER punishment.
  Dominant-driver decision:
    :de-monopolization  entrenched essential chokepoint (load ≥ 0.35) → route-around
    :release            majority commons-bound (readiness ≥ 0.50) → track the 解放 handoff
    :open-license       pledgeable (open-share ≥ 0.20) or concentrated-ish (load ≥ 0.10)
    :monitor            early-life / diversified / low essentiality → watch the clock"
  [f]
  (let [load (exclusivity-load f)
        r (release-readiness f)
        o (double (or (:open-license-share f) 0))]
    (cond
      (>= load 0.35) :de-monopolization
      (>= r 0.50)    :release
      (or (>= o 0.20) (>= load 0.10)) :open-license
      :else          :monitor)))

(defn analyze-field [f]
  (let [ts (top-assignee-share f)]
    {"id" (:id f) "name" (:name f)
     "cpc_section" (:cpc-section f) "cpc_subclass" (:cpc-subclass f)
     "patents_est" (:patents-est f)
     "top_assignee" (top-assignee f)
     "top_assignee_share" ts
     "named_hhi" (named-hhi f)
     "chokepoint_risk" (chokepoint-risk ts)
     "essentiality" (:essentiality f)
     "exclusivity_load" (exclusivity-load f)
     "release_readiness" (release-readiness f)
     "route" (route f)
     "sourcing" (or (:sourcing f) :representative)
     "source" (:source f)}))

;; ── patent release-clock ────────────────────────────────────────────────────
(defn years-to-expiry
  "Statutory years remaining (filing + term − ref-year). nil for pending/no filing."
  ([p] (years-to-expiry p default-ref-year))
  ([p ref-year]
   (when-let [fy (:filing-year p)]
     (- (+ fy (or (:term-years p) 20)) ref-year))))

(defn release-status
  "The patent's position on the RELEASE CLOCK — a DISCLOSED-fact readout, never a
  validity/infringement verdict (G1/G3)."
  ([p] (release-status p default-ref-year))
  ([p ref-year]
   (case (:status p)
     :pending :pending
     (:expired :public-domain) :public-domain
     (:lapsed :withdrawn) :lapsed-released
     ;; granted / in-force: read the clock
     (let [yl (years-to-expiry p ref-year)]
       (cond
         (nil? yl) :in-force
         (<= yl 0) :public-domain
         (<= yl 3) :expiring-soon
         :else :in-force)))))

(defn analyze-patent
  ([p] (analyze-patent p default-ref-year))
  ([p ref-year]
   {"id" (:id p) "title" (:title p) "jurisdiction" (:jurisdiction p)
    "field" (:field p) "assignee" (:assignee p)
    "status" (:status p) "open_license" (or (:open-license p) :none)
    "years_to_expiry" (years-to-expiry p ref-year)
    "release_status" (release-status p ref-year)
    "sourcing" (or (:sourcing p) :representative)
    "source" (:source p)}))

;; ── aggregate ────────────────────────────────────────────────────────────────
(defn- section-aggs [field-rows]
  (->> field-rows
       (group-by #(get % "cpc_section"))
       (map (fn [[sec rows]]
              {"cpc_section" sec
               "count" (count rows)
               "mean_exclusivity_load"
               (round3 (/ (reduce + 0.0 (map #(get % "exclusivity_load") rows)) (count rows)))
               "release_routed"
               (count (filter #(= :release (get % "route")) rows))}))
       (sort-by #(get % "cpc_section"))
       vec))

(defn analyze
  "Full analysis. Takes the classified seed {:fields [...] :patents [...]} (or a flat
  row vector, auto-classified). Optional ref-year for the release clock."
  ([rows] (analyze rows default-ref-year))
  ([rows ref-year]
   (let [{:keys [fields patents]} (if (map? rows)
                                    rows
                                    {:fields (vec (filter #(= (:type %) :field) rows))
                                     :patents (vec (filter #(= (:type %) :patent) rows))})
         farows (mapv analyze-field fields)
         parows (mapv #(analyze-patent % ref-year) patents)]
     {"fields" farows
      "patents" parows
      "sections" (section-aggs farows)})))

;; ── datom emission (append-only EAVT vectors) ───────────────────────────────
(defn datoms
  "Append-only EAVT datom VECTORS for the emission (the persistable form). Every
  entity carries :hirameki/sourcing + :hirameki/derived true (the emission is an
  observation snapshot); :authoritative rows additionally carry :hirameki/source.
  NO :hirameki/infringement-verdict / :hirameki/fto-opinion / :hirameki/equity-signal
  is ever emitted (G1/G3); NO :hirameki.patent/imposes-on (G2) — a patent never
  imposes, only its assignee holds."
  [{:strs [fields patents sections]}]
  (let [fdatoms (mapcat
                 (fn [r]
                   (let [e (str "hirameki-field:" (get r "id"))
                         src (get r "source")]
                     (cond-> [(add e ":hirameki.field/name" (get r "name"))
                              (add e ":hirameki.field/cpc-section" (str (get r "cpc_section")))
                              (add e ":hirameki.field/patents-est" (get r "patents_est"))
                              (add e ":hirameki.obs/top-assignee" (str (get r "top_assignee")))
                              (add e ":hirameki.obs/top-assignee-share" (get r "top_assignee_share"))
                              (add e ":hirameki.obs/named-hhi" (round3 (get r "named_hhi")))
                              (add e ":hirameki.obs/chokepoint-risk" (str (get r "chokepoint_risk")))
                              (add e ":hirameki.obs/exclusivity-load" (round3 (get r "exclusivity_load")))
                              (add e ":hirameki.obs/release-readiness" (round3 (get r "release_readiness")))
                              (add e ":hirameki.obs/route" (str (get r "route")))
                              (add e ":hirameki/sourcing" (str (get r "sourcing")))
                              (add e ":hirameki/derived" true)]
                       src (conj (add e ":hirameki/source" src)))))
                 fields)
        pdatoms (mapcat
                 (fn [r]
                   (let [e (str "hirameki-patent:" (get r "id"))
                         src (get r "source")]
                     (cond-> [(add e ":hirameki.patent/title" (get r "title"))
                              (add e ":hirameki.patent/jurisdiction" (str (get r "jurisdiction")))
                              (add e ":hirameki.patent/field" (get r "field"))
                              (add e ":hirameki.patent/assignee" (str (get r "assignee")))
                              (add e ":hirameki.patent/status" (str (get r "status")))
                              (add e ":hirameki.patent/open-license" (str (get r "open_license")))
                              (add e ":hirameki.obs/release-status" (str (get r "release_status")))
                              (add e ":hirameki/sourcing" (str (get r "sourcing")))
                              (add e ":hirameki/derived" true)]
                       (get r "years_to_expiry")
                       (conj (add e ":hirameki.obs/years-to-expiry" (get r "years_to_expiry")))
                       src (conj (add e ":hirameki/source" src)))))
                 patents)
        sdatoms (mapcat
                 (fn [s]
                   (let [e (str "hirameki-section:" (get s "cpc_section"))]
                     [(add e ":hirameki.section/field-count" (get s "count"))
                      (add e ":hirameki.section/mean-exclusivity-load" (get s "mean_exclusivity_load"))
                      (add e ":hirameki.section/release-routed" (get s "release_routed"))
                      (add e ":hirameki/derived" true)]))
                 sections)]
    (vec (concat fdatoms pdatoms sdatoms))))

(defn render-datoms [assessment]
  (str/join "\n" (map pr-str (datoms assessment))))

;; ── coverage ────────────────────────────────────────────────────────────────
(def ^:private all-sections #{:A :B :C :D :E :F :G :H :Y})
(def ^:private world-patents-est 200000000) ;; ADR-2604251024 framing (~200M public patents)

(defn coverage [{:strs [fields patents]}]
  (let [covered (set (map #(get % "cpc_section") fields))
        have (count covered)
        target (count all-sections)]
    {"by_section" {"covered" (vec (sort (map name covered)))
                   "missing" (vec (sort (map name (set/difference all-sections covered))))}
     "sections_have" have "sections_target" target "sections_gap" (- target have)
     "corpus_have" (count patents)
     "corpus_world_est" world-patents-est
     "corpus_coverage_rate" (round3 (* 100.0 (/ (count patents) (double world-patents-est))))}))

;; ── report (markdown RELEASE MAP — NOT a target-list / FTO / verdict) ───────
;; JVM-only: this builds a markdown table with `format`, which ClojureScript
;; does not have. The namespace is .cljc and the ANALYTICS above are genuinely
;; portable — nbb renders the console from them. This one function never was,
;; and pretending otherwise broke the build the first time nbb loaded the file.
#?(:clj
   (defn render-report [analysis coverage-map]
  (let [fields (get analysis "fields")
        patents (get analysis "patents")
        line (fn [r]
               (format "| %-6s | %-40s | %-3s | %4d%% | %-9s | %.3f | %.3f | %s |"
                       (get r "id")
                       (subs (get r "name") 0 (min 40 (count (get r "name"))))
                       (name (get r "cpc_section"))
                       (get r "top_assignee_share")
                       (name (get r "chokepoint_risk"))
                       (get r "exclusivity_load")
                       (get r "release_readiness")
                       (name (get r "route"))))]
    (str
     "# hirameki 閃き — public-patent RELEASE MAP\n\n"
     "> A RELEASE map (exclusivity → commons), **NOT a target-list**, **NOT an\n"
     "> infringement/FTO/patentability verdict**, **NOT a patent-equity signal** (G1/G3).\n"
     "> A patent is the gated OBJECT, never a holder (G2). Aggregate-first, no person-level\n"
     "> inventor data (G6). Status/shares are disclosed facts, never re-judged.\n\n"
     "## Technology fields (exclusivity-concentration → release route)\n\n"
     "| Field  | Name                                     | Sec | Top  | Chokepoint | Load  | Release | Route |\n"
     "|--------|------------------------------------------|-----|------|-----------|-------|---------|-------|\n"
     (str/join "\n" (map line fields)) "\n\n"
     "## Release clock (exemplar corpus)\n\n"
     "| Patent | Field | Assignee | Status | yrs→expiry | Release |\n"
     "|--------|-------|----------|--------|-----------:|---------|\n"
     (str/join "\n"
               (map (fn [p]
                      (format "| %s | %s | %s | %s | %s | %s |"
                              (get p "id") (get p "field") (name (get p "assignee"))
                              (name (get p "status"))
                              (let [y (get p "years_to_expiry")] (if y (str y) "—"))
                              (name (get p "release_status"))))
                    patents))
     "\n\n## Coverage\n\n"
     (format "- CPC sections: %d/%d (gap %d); missing: %s\n"
             (get coverage-map "sections_have") (get coverage-map "sections_target")
             (get coverage-map "sections_gap")
             (str/join " " (get-in coverage-map ["by_section" "missing"])))
     (format "- Corpus: %d exemplar rows of ~%d world public patents (%.6f%% — the gap the G9 DataLad ingest closes)\n"
             (get coverage-map "corpus_have") (get coverage-map "corpus_world_est")
             (get coverage-map "corpus_coverage_rate"))))))

;; ── CLI (bb runner) ──────────────────────────────────────────────────────────
#?(:clj
   (defn -main [& args]
     (let [seed (or (first args) "kotoba/seed.edn")
           rows (he/load-edn seed)
           classified (he/classify rows)
           analysis (analyze classified)
           cov (coverage analysis)]
       (println (render-report analysis cov)))))
