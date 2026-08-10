(ns hirameki.edge.worker
  "The harvest, as a Cloudflare Worker on a Cron Trigger.

  ## Why this exists

  The collection stopped on 2026-07-28 and nobody noticed for thirteen days.
  It was restarted as a launchd agent on the operator machine, which fixed
  *stopped* but not *single point of failure*: that machine is also where the
  murakumo fleet's own tick runs, so the fleet is not an escape either — it has
  no clock of its own (ADR-2608100400).

  Cloudflare Cron Triggers run on Cloudflare. Measured 2026-08-10 on this
  account: 116 Workers deployed, and `cloud-itonami-domain-reverify` has carried
  a live trigger since 2026-07-26. That is the property that was missing.

  ## What is shared with the operator daemon, and what is not

  SHARED — and this is the point: the pure half of the harvest
  (`hirameki.methods.harvest/next-seed` · `drop-seed` · `defer-seed` ·
  `grow-seeds` · `record-tick`), the parser
  (`toshokan-patents.sources.google-patents`), and the quad codec
  (`toshokan-patents.quad`) are all `.cljc` and compile here unchanged. The seed
  policy, the citation-growth rule and the journal format are one
  implementation, not two that drift.

  NOT SHARED — the effectful edges. The daemon uses a local git checkout;
  this uses the GitHub contents API (`hirameki.edge.github`). Both write the
  same four files, so either can stop and the other continues from where it is.

  ## Politeness is unchanged

  One lookup per tick, sequential, with the same pause between requests. A
  Worker could fan out and will not: the rate is a promise made to the source,
  not a limit of the machine.

  ## What this Worker will NOT do

  Publish the corpus artifacts (`corpus/` and `datoms/` shards with their CIDs).
  That needs `cid/cidv1-raw`, which is JVM-only here, and re-materializing the
  whole corpus on every tick would be wrong anyway. The Worker grows the
  JOURNAL — the authoritative input — and the operator's daemon re-publishes
  the derived artifacts. If the operator machine is off, the journal keeps
  growing and the published shards go stale; that is the correct failure, since
  the journal is the record and the shards are derived from it."
  (:require [clojure.string :as str]
            [cljs.reader :as edn]
            [hirameki.edge.github :as gh]
            [hirameki.methods.harvest :as h]
            [toshokan-patents.quad :as quad]
            [toshokan-patents.sources.google-patents :as gp]))

(def actor-repo "cloud-itonami/hirameki")
(def corpus-repo "cloud-itonami/hirameki-patents")
(def seeds-path "seeds.edn")
(def state-path "state.edn")
(def journal-dir "80-data/public")

(defn- sleep [ms] (js/Promise. (fn [res] (js/setTimeout res ms))))

(defn- shard-path [n]
  (str journal-dir "/google-patents." (subs (str "0000" n) (- (count (str "0000" n)) 4))
       ".journal.edn"))

(defn- active-shard
  "The shard a new quad belongs in: the last one under the size budget, else the
  next index. Mirrors `quad/append-sharded!`, which cannot be used here because
  its file leg is `node:fs` and a Worker has no filesystem.

  Walks forward from 0 rather than listing the directory — a contents listing of
  a directory is one more API call and this is at most a handful of small GETs."
  [token]
  (letfn [(step [i acc]
            (-> (gh/get-file {:token token :repo corpus-repo :path (shard-path i)})
                (.then (fn [f]
                         (cond
                           (nil? f) (if (zero? i)
                                      {:index 0 :file nil :quads [] :prev acc}
                                      {:index (dec i) :file (:prev acc) :quads (:quads acc)})
                           (>= (count (:content f)) quad/default-shard-max-bytes)
                           (step (inc i) {:prev f :quads []})
                           :else {:index i :file f
                                  :quads (edn/read-string (:content f))})))))]
    (step 0 {})))

(defn- read-edn-file [token repo path default]
  (-> (gh/get-file {:token token :repo repo :path path})
      (.then (fn [f] (if f
                       {:data (edn/read-string (:content f)) :sha (:sha f)}
                       {:data default :sha nil})))))

(defn tick!
  "One harvest tick, entirely over the GitHub API. Promise of a summary map.

  Deliberately reads the journal shard fresh each tick rather than caching: the
  operator daemon may have committed between ticks, and a stale read would make
  the optimistic `sha` wrong and lose the write."
  [{:keys [token seeds state at]}]
  (if-let [seed (h/next-seed seeds)]
    (let [pid (:query seed)]
      (-> (active-shard token)
          (.then (fn [shard]
                   (let [existing (:quads shard)
                         entities (quad/entities existing)
                         known (into (set (:dead state))
                                     (map #(str/replace (str %) #"^gp:" ""))
                                     entities)]
                     (-> (gp/lookup pid)
                         (.then
                          (fn [rec]
                            (cond
                              (nil? rec)
                              {:seeds (h/drop-seed seeds seed)
                               :state (h/record-tick state at :dead (str/upper-case (str pid)))
                               :new? false :patent-id pid :reason :not-found}

                              (contains? entities (:entity rec))
                              {:seeds (h/grow-seeds (h/drop-seed seeds seed) (:citations rec)
                                                    known {:max-new-seeds-per-tick 10
                                                           :max-seeds 5000} at)
                               :state (h/record-tick state at)
                               :new? false :patent-id pid :reason :already-known}

                              :else
                              (let [quads (gp/->quads (quad/next-tx existing) at rec)
                                    merged (quad/merge-quads existing quads)]
                                (-> (gh/put-file!
                                     {:token token :repo corpus-repo
                                      :path (shard-path (:index shard))
                                      :sha (get-in shard [:file :sha])
                                      :content (quad/render-journal merged)
                                      :message (str "harvest(worker): " pid
                                                    " +" (count quads) " quads")})
                                    (.then (fn [_]
                                             {:seeds (h/grow-seeds (h/drop-seed seeds seed)
                                                                   (:citations rec)
                                                                   (conj known (str/upper-case (str pid)))
                                                                   {:max-new-seeds-per-tick 10
                                                                    :max-seeds 5000} at)
                                              :state (h/record-tick state at)
                                              :new? true :patent-id pid
                                              :quads (count quads)
                                              :entities (inc (count entities))})))))))
                         (.catch
                          (fn [e]
                            ;; transient: rotate to the back so one bad id cannot
                            ;; hold the head of the queue forever.
                            {:seeds (h/defer-seed seeds seed)
                             :state (h/record-tick state at)
                             :new? false :patent-id pid :reason :fetch-failed
                             :error (ex-message e)}))))))))
    (js/Promise.resolve {:seeds seeds :state (h/record-tick state at)
                         :new? false :reason :frontier-empty})))

(defn run!
  "N ticks, then persist the queue and the state once. Promise of a summary."
  [{:keys [token ticks sleep-ms] :or {ticks 10 sleep-ms 2000}}]
  (-> (js/Promise.all
       #js [(read-edn-file token actor-repo seeds-path {:policy {} :seeds []})
            (read-edn-file token actor-repo state-path {:ticks 0})])
      (.then
       (fn [[seeds-file state-file]]
         (let [seeds0 (vec (:seeds (:data seeds-file)))
               results (atom [])]
           (letfn [(step [i seeds state]
                     (if (> i ticks)
                       (js/Promise.resolve [seeds state])
                       (-> (tick! {:token token :seeds seeds :state state
                                   :at (.toISOString (js/Date.))})
                           (.then (fn [r]
                                    (swap! results conj r)
                                    (if (= i ticks)
                                      [(:seeds r) (:state r)]
                                      ;; pause only BETWEEN requests — the rate is
                                      ;; a promise to the source, not to the clock
                                      (-> (sleep sleep-ms)
                                          (.then (fn [_] (step (inc i) (:seeds r) (:state r)))))))))))]
             (-> (step 1 seeds0 (:data state-file))
                 (.then
                  (fn [[seeds state]]
                    ;; Persist both actor files. A 409 here means the operator
                    ;; daemon wrote first; the journal work already landed, so
                    ;; the next run simply re-reads and continues.
                    (-> (gh/put-file!
                         {:token token :repo actor-repo :path seeds-path
                          :sha (:sha seeds-file)
                          :content (str ";; seeds.edn — pending frontier queue（daemon + worker 管理）\n"
                                        (h/render-edn (assoc (:data seeds-file) :seeds seeds)))
                          :message (str "harvest(worker): queue " (count seeds))})
                        (.then (fn [_]
                                 (gh/put-file!
                                  {:token token :repo actor-repo :path state-path
                                   :sha (:sha state-file)
                                   :content (str ";; state.edn — daemon managed; do not hand-edit.\n"
                                                 (h/render-edn state))
                                   :message (str "harvest(worker): " (count @results) " ticks")})))
                        (.then (fn [_]
                                 {:ticks (count @results)
                                  :new (count (filter :new? @results))
                                  :queue (count seeds)
                                  :dead (count (:dead state))
                                  :reasons (frequencies (map #(or (:reason %) :new) @results))}))))))))))))

(defn- run-and-log [env ticks]
  (when (str/blank? (str (.-GITHUB_TOKEN env)))
    (throw (ex-info (str "GITHUB_TOKEN is not bound — the Worker cannot write. "
                         "Set it with `wrangler secret put GITHUB_TOKEN` (a "
                         "fine-grained PAT with contents:write on "
                         "cloud-itonami/hirameki and cloud-itonami/hirameki-patents "
                         "— NOT a repo-scoped token, which would grant this "
                         "unattended process write access to everything).")
                    {:missing :github-token})))
  (-> (run! {:token (.-GITHUB_TOKEN env)
             :ticks ticks
             :sleep-ms (or (some-> (.-SLEEP_MS env) js/parseInt) 2000)})
      (.then (fn [r]
               (js/console.log (js/JSON.stringify (clj->js r)))
               r))
      (.catch (fn [e]
                ;; Loud, and rethrown: a Worker that swallows its error looks
                ;; identical to one that had nothing to do.
                (js/console.error "hirameki-harvest worker failed:" (ex-message e))
                (throw e)))))

(def app
  #js {:scheduled
       (fn [_event env ctx]
         (.waitUntil ctx (run-and-log env (or (some-> (.-TICKS env) js/parseInt) 10))))

       ;; A manual trigger, so the thing can be exercised without waiting for
       ;; cron. Requires the same token, so it is not an open endpoint.
       :fetch
       (fn [^js req env _ctx]
         (let [url (js/URL. (.-url req))
               auth (.get (.-headers req) "authorization")]
           (cond
             (not= (.-pathname url) "/run")
             (js/Response. "hirameki harvest worker — POST /run\n" #js {:status 404})

             ;; **Fail closed when the secret is absent.** With no RUN_TOKEN
             ;; bound, `(str "Bearer " nil)` is the literal string
             ;; "Bearer " — and an unauthenticated caller sending exactly
             ;; that header would have matched. A missing secret must mean
             ;; "nobody can call this", never "anybody can".
             (or (str/blank? (str (.-RUN_TOKEN env)))
                 (= "undefined" (str (.-RUN_TOKEN env)))
                 (not= auth (str "Bearer " (.-RUN_TOKEN env))))
             (js/Response. "unauthorized\n" #js {:status 401})

             :else
             (-> (run-and-log env (or (some-> (.get (.-searchParams url) "ticks") js/parseInt) 5))
                 (.then (fn [r] (js/Response. (str (js/JSON.stringify (clj->js r)) "\n")
                                              #js {:status 200})))
                 (.catch (fn [e] (js/Response. (str "failed: " (ex-message e) "\n")
                                               #js {:status 500})))))))})
