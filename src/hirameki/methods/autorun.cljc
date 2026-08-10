(ns hirameki.methods.autorun
  "hirameki 閃き — the observatory heartbeat.

  One beat: read the curated field seed AND the harvested patent corpus, run the
  release analysis over both, and append the derived observation datoms as ONE
  content-addressed tx to the append-only ledger. `prev-cid` chaining keeps the
  ledger tamper-evident and resume-safe.

  ## Idempotent by content, not by clock

  A beat whose datoms equal the previous beat's is a NO-OP. The ledger records
  CHANGES, so a loop running every six hours over a corpus that grew by nothing
  appends nothing. This is why the observatory registration carries
  `:change-rate-basis` — the honest statement is 'this grows when the corpus
  grows', not 'this grows every tick'.

  Deterministic: the caller supplies `tx-id` and `as-of`; there is no wall clock
  and no `Math/random` in the beat, so a replay from the same inputs produces
  the same chain.

  OBSERVATION ONLY — hirameki never files, never litigates, never trades, and
  never issues an FTO or infringement verdict (G1). ADR-2606212200."
  (:require [hirameki.methods.analyze :as a]
            [hirameki.methods.corpus :as c]
            [hirameki.methods.kotoba :as k]
            #?(:clj [toshokan-patents.quad :as quad])
            #?(:clj [hirameki.methods.hirameki-edn :as he])
            #?(:clj [clojure.java.io :as io])))

(defn observation-datoms
  "The full datom set for one beat: curated-field analytics + harvested-corpus
  aggregates. Pure.

  `seed-rows` carry real CPC sections, so they produce field-level concentration.
  `corpus-rows` do not (a Google Patents page has no CPC), so they produce
  corpus-level aggregates instead. Keeping them separate is deliberate — merging
  them would let CPC-less rows silently dilute a field's numbers."
  [seed-rows corpus-rows ref-year]
  (let [assessment (a/analyze seed-rows ref-year)
        summary (when (seq corpus-rows) (c/summarize corpus-rows ref-year))]
    {:assessment assessment
     :summary summary
     :datoms (into (vec (a/datoms assessment))
                   (when summary (c/summary-datoms summary)))}))

(defn beat
  "Run one heartbeat. opts:
     :seed-rows    classified curated seed `{:fields [...] :patents [...]}` (required)
     :corpus-rows  harvested patent rows (optional; `[]` when the corpus is absent)
     :ref-year     deterministic release-clock reference year
     :tx-id        deterministic tx id (required)
     :as-of        deterministic as-of stamp (required)
     :log-path     ledger path (required)
   Returns a summary map including `:appended` and, when nothing changed,
   `:reason :no-change`."
  [{:keys [seed-rows corpus-rows ref-year tx-id as-of log-path]
    :or {ref-year a/default-ref-year corpus-rows []}}]
  (let [{:keys [assessment summary datoms]}
        (observation-datoms seed-rows corpus-rows ref-year)
        prev (k/head-cid log-path)
        last-ds (let [txs (k/read-log log-path)]
                  (when (seq txs) (:tx/datoms (last txs))))
        base {:count (count datoms)
              :fields (count (get assessment "fields"))
              :patents (count (get assessment "patents"))
              :sections (count (get assessment "sections"))
              :corpus (count corpus-rows)
              :frontier (get-in summary ["citations" "frontier"] 0)}]
    (if (= datoms last-ds)
      (assoc base :head prev :appended false :reason :no-change)
      (let [tx (k/make-tx datoms tx-id as-of prev)]
        (assoc base :head (k/append-tx tx log-path) :appended true :reason nil)))))

;; ── corpus loading (file leg) ────────────────────────────────────────────────

#?(:clj
   (defn load-corpus
     "Harvested patent rows from the corpus repo's journal. `[]` when the repo is
     not present — a missing corpus degrades the beat to seed-only analytics
     rather than failing it, and the beat reports `:corpus 0` so the gap is
     visible instead of being mistaken for an empty world."
     [dataset-repo]
     (if (seq dataset-repo)
       (let [f (io/file dataset-repo "80-data" "public" "google-patents.journal.edn")]
         (if (.exists f)
           (c/journal->patents (quad/read-journal (str f)))
           []))
       [])))

#?(:clj
   (defn run-cycles
     "N beats over the same inputs. After the first, every further beat is a
     no-op unless the corpus changed underneath — which is the point."
     [{:keys [cycles seed-path dataset-repo log-path ref-year as-of-base]
       :or {cycles 1 seed-path "kotoba/seed.edn" as-of-base 0}}]
     (let [seed-rows (he/classify (he/load-edn seed-path))
           corpus-rows (load-corpus dataset-repo)
           beats (mapv (fn [i]
                         (beat {:seed-rows seed-rows
                                :corpus-rows corpus-rows
                                :ref-year (or ref-year a/default-ref-year)
                                :tx-id (str "hirameki-beat-" i)
                                :as-of (+ as-of-base i)
                                :log-path log-path}))
                       (range 1 (inc cycles)))]
       {:cycles cycles
        :beats beats
        :log-length (count (k/read-log log-path))
        :head (k/head-cid log-path)
        :chain (k/verify-chain log-path)})))

#?(:clj
   (defn -main
     "CLI entry — the observatory runner shape (`manifest/observatories.edn`).

       clojure -M -m hirameki.methods.autorun --cycles 1 --log data/hirameki.datoms.kotoba.edn"
     [& argv]
     (let [argv (vec argv)
           arg (fn [flag dflt] (let [i (.indexOf argv flag)]
                                 (if (>= i 0) (nth argv (inc i)) dflt)))
           cycles (Long/parseLong (arg "--cycles" "1"))
           log-path (arg "--log" "data/hirameki.datoms.kotoba.edn")
           dataset (arg "--dataset" (System/getenv "HIRAMEKI_DATASET_REPO"))
           seed-path (arg "--seed" "kotoba/seed.edn")
           ref-year (when-let [v (arg "--ref-year" nil)] (Long/parseLong v))]
       (io/make-parents (io/file log-path))
       (when (and (some #{"--fresh"} argv) (.exists (io/file log-path)))
         (.delete (io/file log-path)))
       (let [res (run-cycles {:cycles cycles :seed-path seed-path :dataset-repo dataset
                              :log-path log-path :ref-year ref-year})]
         (println "# hirameki 閃き — public-patent RELEASE observation (exclusivity → commons)")
         (println "#   a release map, never an FTO / infringement / patent-equity verdict (G1)\n")
         (doseq [b (:beats res)]
           (println (str "  ♦ " (:fields b) " fields / " (:patents b) " seed patents / "
                         (:corpus b) " harvested · citation frontier " (:frontier b)
                         " · " (:count b) " datoms → "
                         (if (:appended b)
                           (str "cid " (subs (str (:head b)) 0 (min 14 (count (str (:head b))))) "…")
                           "no change"))))
         ;; `verify-chain` returns KEYWORD keys ({:ok :length :broken-at}). Reading
         ;; it with string keys — the shape a sibling actor happens to use — makes
         ;; every healthy ledger print "BROKEN", which is worse than not checking:
         ;; a permanently-red signal gets ignored.
         (let [ch (:chain res)]
           (println (str "\n  ledger: " (:log-length res) " tx · head "
                         (subs (str (:head res)) 0 (min 14 (count (str (:head res))))) "… · chain "
                         (if (:ok ch) "OK" (str "BROKEN at tx " (:broken-at ch)))))
           (when-not (:ok ch) (System/exit 1)))))))
