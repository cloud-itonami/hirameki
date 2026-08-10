(ns hirameki.methods.harvest
  "hirameki 閃き — the self-growing patent harvest leg (G9 live, key-free).

  One tick: take the next un-exhausted seed, look it up on Google Patents,
  append the new bibliographic quads to the corpus journal, and grow the seed
  list from the patents that one cites. The corpus explores the citation graph
  by walking it.

  ## Where the residency lives, and why it moved

  Until 2026-08-10 this loop lived in `kotoba-lang/toshokan-patents` next to the
  parser. It ran 618 patents deep and then stopped on 2026-07-28 — nobody
  noticed for thirteen days, because a daemon inside a library repo is nobody's
  actor. kotoba-lang now holds the PARSER as a library
  (`toshokan-patents.sources.google-patents`, pure + one request) and this actor
  holds the LOOP, the seed policy, the cursor, and the gates.

  ## Discipline

  - one lookup per tick, sequential, identifying User-Agent (in the library)
  - metadata only — never claims or specification text
  - a 404 is a normal outcome (dead citation), not an error: the pair is marked
    exhausted and the cursor moves on
  - `:max-seeds` bounds the frontier so a citation graph cannot grow unbounded

  ## Gates enforced here

  - **G6** the journal records what the page published, including inventors;
    the CORPUS BOUNDARY (`corpus.cljc`) is where person-level data is dropped.
    Nothing in this namespace derives a person-level aggregate.
  - **G3** fields are mirrored as disclosed, never re-judged.
  - **G8** no key, no credential, no account — this source needs none, which is
    the whole reason it was chosen over USPTO ODP and EPO OPS.

  Deterministic where it can be: `tick!` takes `retrieved-at` from the caller,
  so a replay writes the same quads."
  (:require [clojure.string :as str]
            [toshokan-patents.quad :as quad]
            [toshokan-patents.sources.google-patents :as gp]
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def default-policy
  {:max-new-seeds-per-tick 10
   :max-seeds 5000
   :inter-request-sleep-ms 2000})

;; ── pure: seed bookkeeping ───────────────────────────────────────────────────

(defn next-seed
  "The next seed to work, round-robin from `:cursor`, skipping exhausted ids.
  nil when every seed is exhausted. Pure."
  [{:keys [cursor exhausted] :or {cursor 0}} seeds]
  (let [n (count seeds)
        done (set exhausted)]
    (when (pos? n)
      (some (fn [i]
              (let [idx (mod (+ cursor i) n)
                    s (nth seeds idx)]
                (when-not (contains? done (:id s))
                  {:seed s :index idx})))
            (range n)))))

(defn grow-seeds
  "Append seeds for the patents `record` cites, bounded by policy. Pure.

  This is what makes the corpus self-growing: every harvested patent hands back
  the ids it references, and those become the next frontier. Already-known
  queries are skipped, so the graph converges rather than cycling."
  [seeds citations {:keys [max-new-seeds-per-tick max-seeds]} grown-at]
  (let [known (into #{} (map (comp str/upper-case str :query)) seeds)
        room (max 0 (- max-seeds (count seeds)))
        fresh (->> citations
                   (map str/upper-case)
                   (remove str/blank?)
                   (remove #(> (count %) 30))
                   (remove known)
                   distinct
                   (take (min max-new-seeds-per-tick room)))]
    (into (vec seeds)
          (map (fn [pid]
                 {:id (str "cited-" (str/lower-case pid))
                  :query pid
                  :grown-from "google-patents"
                  :grown-at grown-at}))
          fresh)))

(defn advance
  "State after working `index`: cursor past it, seed id marked exhausted.

  Every lookup exhausts its seed — one patent id resolves to at most one page,
  so there is no pagination to advance and no reason to ask twice. Pure."
  [state index seed-id at]
  (-> state
      (assoc :cursor (inc index) :last-tick at)
      (update :ticks (fnil inc 0))
      (update :exhausted (fnil conj #{}) seed-id)))

;; ── effectful: one tick ──────────────────────────────────────────────────────

#?(:clj
   (defn tick!
     "One harvest tick against a corpus journal. Returns a summary map.

     opts:
       :journal-path  corpus journal file (required)
       :seeds         current seed vector (required)
       :state         current state map (required)
       :policy        merged over `default-policy`
       :retrieved-at  timestamp stamped into the quads (required — the caller
                      owns the clock, so a replay is reproducible)

     Returns `{:seeds :state :quads :new? :patent-id :citations :reason}`.
     Writes the journal; does NOT write seeds/state/git — the caller does, so a
     failed write cannot leave the cursor ahead of the data."
     [{:keys [journal-path seeds state policy retrieved-at]}]
     (let [policy (merge default-policy policy)]
       (if-let [{:keys [seed index]} (next-seed state seeds)]
         (let [pid (:query seed)
               existing (quad/read-journal journal-path)
               known (quad/entities existing)
               rec (try (gp/lookup pid)
                        (catch Exception e
                          (ex-info (str "lookup failed for " pid) {:patent-id pid} e)))]
           (cond
             (instance? clojure.lang.ExceptionInfo rec)
             {:seeds seeds
              :state (assoc state :last-tick retrieved-at)
              :new? false :patent-id pid :reason :fetch-failed
              :error (ex-message rec)}

             (nil? rec)
             {:seeds seeds
              :state (advance state index (:id seed) retrieved-at)
              :new? false :patent-id pid :reason :not-found}

             (contains? known (:entity rec))
             {:seeds (grow-seeds seeds (:citations rec) policy retrieved-at)
              :state (advance state index (:id seed) retrieved-at)
              :new? false :patent-id pid :reason :already-known
              :citations (count (:citations rec))}

             :else
             (let [quads (gp/->quads (quad/next-tx existing) retrieved-at rec)]
               (quad/append-journal! journal-path quads)
               {:seeds (grow-seeds seeds (:citations rec) policy retrieved-at)
                :state (advance state index (:id seed) retrieved-at)
                :quads (count quads)
                :new? true :patent-id pid
                :citations (count (:citations rec))
                :entities (inc (count known))})))
         {:seeds seeds
          :state (assoc state :last-tick retrieved-at)
          :new? false :reason :seeds-exhausted}))))

;; ── file legs ────────────────────────────────────────────────────────────────

#?(:clj
   (defn read-edn-file [path default]
     (let [f (io/file path)]
       (if (.exists f) (edn/read-string (slurp f)) default))))

#?(:clj
   (defn write-edn-file! [path data comment]
     (spit (io/file path) (str ";; " comment "\n" (pr-str data) "\n"))))

#?(:clj
   (defn run-ticks!
     "N ticks against a corpus journal, persisting seeds and state after each.

     `:dataset-repo` is the cloud-itonami/hirameki-patents checkout — the corpus
     is a separate, independently versioned repository (ADR-2606212200), so this
     actor writes into it rather than carrying the data itself."
     [{:keys [ticks dataset-repo seeds-path state-path retrieved-at sleep-ms]
       :or {ticks 1 seeds-path "seeds.edn" state-path "state.edn"}}]
     (when-not (seq dataset-repo)
       (throw (ex-info "dataset repo path required (--dataset or HIRAMEKI_DATASET_REPO)" {})))
     (let [journal-path (str (io/file dataset-repo "80-data" "public" "google-patents.journal.edn"))
           seeds-file (read-edn-file seeds-path {:policy {} :seeds []})
           policy (merge default-policy (:policy seeds-file))]
       (io/make-parents (io/file journal-path))
       (loop [i 1
              seeds (vec (:seeds seeds-file))
              state (read-edn-file state-path {:cursor 0 :exhausted #{} :ticks 0})
              results []]
         (if (> i ticks)
           (do (write-edn-file! seeds-path (assoc seeds-file :seeds seeds)
                                "seeds.edn — daemon-managed (citation-graph self-grow) + human edits.")
               (write-edn-file! state-path state
                                "state.edn — daemon managed; do not hand-edit.")
               {:ticks (count results)
                :new (count (filter :new? results))
                :seeds (count seeds)
                :results results})
           (let [r (tick! {:journal-path journal-path :seeds seeds :state state
                           :policy policy
                           :retrieved-at (or retrieved-at
                                             (str (java.time.Instant/now)))})]
             (println (str "  ♦ tick " i "/" ticks " " (:patent-id r)
                           (if (:new? r)
                             (str " → +" (:quads r) " quads, " (:citations r) " citations"
                                  " (corpus " (:entities r) ")")
                             (str " → " (name (or (:reason r) :no-op))))))
             (when-let [ms (or sleep-ms (:inter-request-sleep-ms policy))]
               (when (< i ticks) (Thread/sleep ms)))
             (recur (inc i) (:seeds r) (:state r) (conj results r))))))))

#?(:clj
   (defn -main
     "CLI: N harvest ticks into the corpus repo.

       clojure -M -m hirameki.methods.harvest --ticks 20 --dataset ../hirameki-patents"
     [& argv]
     (let [argv (vec argv)
           arg (fn [flag dflt] (let [i (.indexOf argv flag)]
                                 (if (>= i 0) (nth argv (inc i)) dflt)))
           ticks (Long/parseLong (arg "--ticks" "1"))
           dataset (arg "--dataset" (System/getenv "HIRAMEKI_DATASET_REPO"))
           r (run-ticks! {:ticks ticks :dataset-repo dataset})]
       (println (str "\nharvest: " (:ticks r) " ticks · " (:new r) " new patents · "
                     (:seeds r) " seeds queued")))))
