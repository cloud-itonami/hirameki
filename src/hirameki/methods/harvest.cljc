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
            #?(:clj [toshokan-patents.quad.fs :as qfs])
            [toshokan-patents.sources.google-patents :as gp]
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])))

(def default-policy
  {:max-new-seeds-per-tick 10
   :max-seeds 5000
   :inter-request-sleep-ms 2000})

;; ── pure: the frontier is a QUEUE, not an append-only list ──────────────────
;;
;; It used to be a list that only grew, with a parallel `:exhausted` set that
;; also only grew, and `:max-seeds` capping the LIFETIME number of seeds ever
;; created. That is the wrong thing to bound, and it put a hard ceiling on the
;; corpus: measured 2026-08-10 at 1,812 seeds / 688 worked, the cap of 5,000
;; would have been reached in ~10 days and the loop would have run dry ~26 days
;; later at ~5,000 patents, permanently. Not a bug — a bound on the wrong noun.
;;
;; Now `seeds` holds only what has NOT been worked. A seed is removed when it is
;; worked, so the queue drains as fast as it fills and `:max-seeds` bounds the
;; OUTSTANDING frontier, which is what a politeness/memory bound should mean.
;; Both files stop growing without end, which is also the answer to seeds.edn
;; being a 206 KB single line rewritten on every tick.
;;
;; The only thing that must be remembered forever is which ids are DEAD (404) —
;; a patent id that does not exist will not exist later either, and without that
;; memory the citation graph would re-offer it every time it is cited again.
;; Harvested ids need no memory at all: the corpus itself is the record.

(defn next-seed
  "The seed at the head of the queue, or nil when the frontier is empty. Pure."
  [seeds]
  (first seeds))

(defn drop-seed
  "Remove `seed` from the queue — it has been worked. Pure."
  [seeds seed]
  (vec (remove #(= (:id %) (:id seed)) seeds)))

(defn defer-seed
  "Move `seed` to the BACK of the queue instead of dropping it.

  For a transient failure (network, rate limit). Rotating rather than leaving it
  at the head matters: a single id that always fails would otherwise be retried
  forever at position 0 and the loop would make no progress at all while
  reporting a healthy exit."
  [seeds seed]
  (conj (drop-seed seeds seed) seed))

(defn grow-seeds
  "Append seeds for the patents `record` cites, bounded by policy. Pure.

  This is what makes the corpus self-growing: every harvested patent hands back
  the ids it references, and those become the next frontier.

  `known` is every id there is no point queueing — already harvested (taken from
  the corpus, which is the record) or known dead. Deduping against the corpus
  rather than against a remembered list of worked seeds is what lets the worked
  list be thrown away."
  [seeds citations known {:keys [max-new-seeds-per-tick max-seeds]} grown-at]
  (let [queued (into #{} (map (comp str/upper-case str :query)) seeds)
        room (max 0 (- max-seeds (count seeds)))
        fresh (->> citations
                   (map str/upper-case)
                   (remove str/blank?)
                   (remove #(> (count %) 30))
                   (remove queued)
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

(defn record-tick
  "State after one tick. Pure. Keeps counters and the dead set — nothing that
  grows once per seed ever worked."
  [state at & {:keys [dead]}]
  (cond-> (-> state
              (assoc :last-tick at)
              (update :ticks (fnil inc 0)))
    dead (update :dead (fnil conj #{}) dead)))

(defn migrate-state
  "One-time: the old `{:exhausted #{...}}` shape → the queue shape.

  Two things get removed from the queue, and the second is the one that matters:

  1. seeds named in `:exhausted`. A worked seed whose patent is NOT in the
     corpus was a 404, so it becomes `:dead` — losing that distinction would
     make the citation graph re-offer every dead id forever.

  2. **any seed whose patent is already in the corpus**, whatever `:exhausted`
     says. The original daemon wrote `:exhausted` as `\"source|seed|p1\"` pair
     keys, not seed ids, so matching on ids alone left ~618 already-harvested
     seeds in the queue — each of which would burn a tick and a request to learn
     `already-known`. Deduping against the corpus is both the repair and the
     rule: the corpus is the record of what has been fetched, so anything in it
     does not belong in a queue of what to fetch.

  Idempotent, and safe to keep running: (2) is a useful invariant every time,
  not only during migration."
  [{:keys [policy seeds]} state harvested-queries]
  (let [done (set (:exhausted state))
        q-of #(str/upper-case (str (:query %)))
        worked (filter #(done (:id %)) seeds)
        dead (into (set (:dead state))
                   (comp (map q-of) (remove harvested-queries))
                   worked)
        pending (vec (remove #(or (done (:id %))
                                  (harvested-queries (q-of %))
                                  (dead (q-of %)))
                             seeds))]
    {:seeds {:policy policy :seeds pending}
     :state (cond-> (-> state
                        (dissoc :exhausted :cursor :pages)
                        (assoc :dead dead))
              (seq done) (assoc :migrated-at (:last-tick state)))
     :dropped (- (count seeds) (count pending))}))

;; ── effectful: one tick ──────────────────────────────────────────────────────

#?(:clj
   (defn tick!
     "One harvest tick against a corpus journal. Returns a summary map.

     opts:
       :journal-dir   corpus journal directory (required)
       :seeds         the pending queue (required)
       :state         current state map (required)
       :policy        merged over `default-policy`
       :retrieved-at  timestamp stamped into the quads (required — the caller
                      owns the clock, so a replay is reproducible)

     Writes the journal; does NOT write seeds/state/git — the caller does, so a
     failed write cannot leave the queue ahead of the data."
     [{:keys [journal-dir seeds state policy retrieved-at]}]
     (let [policy (merge default-policy policy)]
       (if-let [seed (next-seed seeds)]
         (let [pid (:query seed)
               existing (qfs/read-sharded journal-dir "google-patents")
               entities (quad/entities existing)
               ;; ids there is no point queueing again: already in the corpus,
               ;; or known dead. This is what replaces the old `:exhausted` list.
               known (into (set (:dead state))
                           (map #(str/replace (str %) #"^gp:" ""))
                           entities)
               rec (try (gp/lookup pid)
                        (catch Exception e
                          (ex-info (str "lookup failed for " pid) {:patent-id pid} e)))]
           (cond
             (instance? clojure.lang.ExceptionInfo rec)
             ;; transient: rotate to the back rather than dropping the fact that
             ;; it is still owed, and rather than retrying it at position 0.
             {:seeds (defer-seed seeds seed)
              :state (record-tick state retrieved-at)
              :new? false :patent-id pid :reason :fetch-failed
              :error (ex-message rec)}

             (nil? rec)
             {:seeds (drop-seed seeds seed)
              :state (record-tick state retrieved-at :dead (str/upper-case (str pid)))
              :new? false :patent-id pid :reason :not-found}

             (contains? entities (:entity rec))
             {:seeds (grow-seeds (drop-seed seeds seed) (:citations rec) known policy retrieved-at)
              :state (record-tick state retrieved-at)
              :new? false :patent-id pid :reason :already-known
              :citations (count (:citations rec))}

             :else
             (let [quads (gp/->quads (quad/next-tx existing) retrieved-at rec)]
               ;; sharded: only the bounded active shard is rewritten, so the cost
               ;; of a tick does not grow with the size of the corpus.
               (qfs/append-sharded! journal-dir "google-patents" quads)
               {:seeds (grow-seeds (drop-seed seeds seed) (:citations rec)
                                   (conj known (str/upper-case (str pid))) policy retrieved-at)
                :state (record-tick state retrieved-at)
                :quads (count quads)
                :new? true :patent-id pid
                :citations (count (:citations rec))
                :entities (inc (count entities))})))
         {:seeds seeds
          :state (record-tick state retrieved-at)
          :new? false :reason :frontier-empty}))))

;; ── file legs ────────────────────────────────────────────────────────────────

#?(:clj
   (defn read-edn-file [path default]
     (let [f (io/file path)]
       (if (.exists f) (edn/read-string (slurp f)) default))))

(defn render-edn
  "EDN with the seed vector ONE PER LINE, still a single readable form.

  `seeds.edn` was a single 206 KB line rewritten on every tick — the same shape
  that made the journal undiffable and its git blobs unreviewable. The queue is
  bounded now so size is no longer the problem, but a one-line file still makes
  `git diff` useless on the file that records what the loop is about to do next.

  It stays ONE map so `edn/read-string` reads it unchanged; only the whitespace
  inside the `:seeds` vector differs.

  **Unconditional, not `:clj`-only.** The Cloudflare Worker writes the same two
  files through the GitHub API, so a JVM-only writer would mean two formats for
  one file — and the one that is easier to write is the one-line form this
  function exists to avoid."
  [data]
  (if-let [seeds (:seeds data)]
    (let [head (pr-str (dissoc data :seeds))]
      (str (subs head 0 (dec (count head)))            ; drop the closing }
           (when (> (count head) 2) " ")
           ":seeds ["
           (reduce str (map #(str "\n" (pr-str %)) seeds))
           "\n]}\n"))
    (str (pr-str data) "\n")))

#?(:clj
   (defn write-edn-file! [path data comment]
     (spit (io/file path) (str ";; " comment "\n" (render-edn data)))))

#?(:clj
   (defn run-ticks!
     "N ticks against a corpus journal, persisting the queue and state at the end.

     `:dataset-repo` is the cloud-itonami/hirameki-patents checkout — the corpus
     is a separate, independently versioned repository (ADR-2606212200), so this
     actor writes into it rather than carrying the data itself.

     Migrates the old append-only seed shape on first run."
     [{:keys [ticks dataset-repo seeds-path state-path retrieved-at sleep-ms]
       :or {ticks 1 seeds-path "seeds.edn" state-path "state.edn"}}]
     (when-not (seq dataset-repo)
       (throw (ex-info "dataset repo path required (--dataset or HIRAMEKI_DATASET_REPO)" {})))
     (let [journal-dir (str (io/file dataset-repo "80-data" "public"))
           _ (.mkdirs (io/file journal-dir))
           harvested (into #{}
                           (map #(str/upper-case (str/replace (str %) #"^gp:" "")))
                           (quad/entities (qfs/read-sharded journal-dir "google-patents")))
           {:keys [seeds state dropped]} (migrate-state
                                  (read-edn-file seeds-path {:policy {} :seeds []})
                                  (read-edn-file state-path {:ticks 0})
                                  harvested)
           policy (merge default-policy (:policy seeds))
           seeds-file seeds]
       (when (pos? (or dropped 0))
         (println (str "  · queue から " dropped " 件を除去"
                       "（作業済み / 取得済み）· dead " (count (:dead state)) " 件")))
       (loop [i 1, q (vec (:seeds seeds-file)), st state, results []]
         (if (> i ticks)
           (do (write-edn-file! seeds-path (assoc seeds-file :seeds q)
                                "seeds.edn — pending frontier queue（daemon 管理 + 人手編集）")
               (write-edn-file! state-path st
                                "state.edn — daemon managed; do not hand-edit.")
               {:ticks (count results)
                :new (count (filter :new? results))
                :queue (count q)
                :dead (count (:dead st))
                :results results})
           (let [r (tick! {:journal-dir journal-dir :seeds q :state st
                           :policy policy
                           :retrieved-at (or retrieved-at (str (java.time.Instant/now)))})]
             (println (str "  ♦ tick " i "/" ticks " " (:patent-id r)
                           (if (:new? r)
                             (str " → +" (:quads r) " quads, " (:citations r) " citations"
                                  " (corpus " (:entities r) ")")
                             (str " → " (name (or (:reason r) :no-op))))
                           " · queue " (count (:seeds r))))
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
                     "queue " (:queue r) " · dead " (:dead r))))))
