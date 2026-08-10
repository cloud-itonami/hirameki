(ns hirameki.edge.github
  "The GitHub contents API as the Worker's filesystem.

  A Cloudflare Worker has no disk and no git binary, but the harvest's whole
  persistent state is four files in two repositories — the seed queue, the
  cursor state, the journal shard, and (on the corpus side) the published
  artifacts. So the Worker reads and writes them through the contents API, which
  is exactly the path this project's commits have been taking by hand all along.

  ## Why not KV or D1

  Because then the Worker and the operator daemon would have DIFFERENT state,
  and the corpus would depend on which one ran last. Keeping state in git means
  the two are interchangeable: either can stop, the other continues from the
  same files, and the history stays reviewable. It also keeps the promise that
  the git history is the authoritative record (ADR-2607072300) — a Worker with
  its own KV would quietly become a second source of truth.

  ## Optimistic concurrency is free here

  `PUT /contents` takes the blob `sha` it expects to replace and returns 409 if
  the file moved underneath. That is the whole conflict story: if the operator
  daemon committed between this Worker's read and write, the write fails loudly
  and the tick is retried next cron. No locking, no lease, no coordination —
  and no chance of two writers silently clobbering each other."
  (:require [clojure.string :as str]))

(def api "https://api.github.com")

(defn- headers [token]
  #js {"Authorization" (str "Bearer " token)
       "Accept" "application/vnd.github+json"
       "X-GitHub-Api-Version" "2022-11-28"
       "User-Agent" "hirameki-harvest-worker (cloud-itonami/hirameki)"
       "Content-Type" "application/json"})

(defn- b64->str
  "GitHub returns base64 with newlines; atob does not tolerate them.
  The content is UTF-8, and `atob` yields latin1, so it has to be decoded
  properly or every Japanese applicant name in the journal becomes mojibake."
  [b64]
  (let [clean (str/replace b64 #"\s" "")
        bin (js/atob clean)
        bytes (js/Uint8Array.from bin #(.charCodeAt % 0))]
    (.decode (js/TextDecoder. "utf-8") bytes)))

(defn- str->b64 [s]
  (let [bytes (.encode (js/TextEncoder.) s)]
    (js/btoa (.apply js/String.fromCharCode nil bytes))))

(defn get-file
  "Returns a Promise of `{:content <string> :sha <blob-sha>}`, or nil if absent.

  `:sha` is what a later `put-file!` must hand back — it is the optimistic lock."
  [{:keys [token repo path ref]}]
  (-> (js/fetch (str api "/repos/" repo "/contents/" path
                     (when ref (str "?ref=" ref)))
                #js {:headers (headers token)})
      (.then (fn [^js r]
               (cond
                 (= 404 (.-status r)) nil
                 (.-ok r) (-> (.json r)
                              (.then (fn [^js j]
                                       {:content (b64->str (.-content j))
                                        :sha (.-sha j)})))
                 :else (-> (.text r)
                           (.then (fn [t]
                                    (throw (ex-info (str "GitHub GET " (.-status r) " " path)
                                                    {:status (.-status r) :body (subs t 0 300)}))))))))))

(defn put-file!
  "Commit one file. `:sha` is the blob being replaced (omit to create).

  Returns a Promise of `{:commit <sha>}`, or throws with `:conflict true` on 409
  — which means someone else wrote first and this tick should be abandoned
  rather than retried in place."
  [{:keys [token repo path content message sha branch]}]
  (-> (js/fetch (str api "/repos/" repo "/contents/" path)
                #js {:method "PUT"
                     :headers (headers token)
                     :body (js/JSON.stringify
                            (clj->js (cond-> {:message message
                                              :content (str->b64 content)}
                                       sha (assoc :sha sha)
                                       branch (assoc :branch branch))))})
      (.then (fn [^js r]
               (if (.-ok r)
                 (-> (.json r) (.then (fn [^js j] {:commit (.. j -commit -sha)})))
                 (-> (.text r)
                     (.then (fn [t]
                              (throw (ex-info (str "GitHub PUT " (.-status r) " " path)
                                              {:status (.-status r)
                                               :conflict (= 409 (.-status r))
                                               :body (subs t 0 300)}))))))))))
