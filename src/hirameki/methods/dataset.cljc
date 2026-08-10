(ns hirameki.methods.dataset
  "Deterministic patent corpus materialization into the independent dataset repository.

  ## Why the artifacts are sharded

  A `bafkrei…` CIDv1/raw CID is the hash of ONE raw block, and it equals what
  `ipfs add --cid-version=1 --raw-leaves` produces only while the file stays
  under the 256 KiB chunker limit. Past that, IPFS builds a UnixFS DAG and the
  root is a `bafybei…` dag-pb CID — a completely different value.

  Measured 2026-08-10, when the corpus grew from 7 rows to 625:

      corpus 280,078 bytes
        claimed  bafkreibo345qp6u5zkpf3zrbp4hbwtl6u5ov53gsixb43azzgnw7svk3va
        ipfs add bafybeicpvod7rmqy2l74m32tjf3gtaez6q2axx5pznwofi4aaeedlcy3pm

  So the repo's promise — *fetch from any gateway and verify without trusting a
  daemon* — silently became false the moment the corpus outgrew one block. Two
  ways out: build a real UnixFS DAG (which would make publishing depend on an
  IPFS implementation, defeating the point), or keep every artifact inside one
  block. This takes the second: rows are packed into shards under the limit,
  and the manifest carries a CID per shard. Verification stays a `sha256` of
  bytes you already hold."
  (:require [hirameki.methods.analyze :as a]
            [hirameki.methods.cid :as cid]
            #?(:clj [hirameki.methods.corpus :as corpus])
            #?(:clj [hirameki.methods.normalize :as nz])
            #?(:clj [toshokan-patents.quad :as quad])
            #?(:clj [hirameki.methods.hirameki-edn :as he])
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.pprint :as pprint])))

(def corpus-dir "corpus")
(def datoms-dir "datoms")

;; Leave room under the 256 KiB block limit for the wrapping brackets and the
;; trailing newline; a shard that lands one byte over would be a DIFFERENT kind
;; of CID, not a slightly bigger one.
(def shard-budget (- cid/single-block-limit 8192))

(defn- normalize-patent [p]
  ;; Preserve the source map's stable EDN field order; omit actor-only classification/notes.
  (dissoc p :type :note))

(defn- utf8-count [^String s]
  #?(:clj (alength (.getBytes s "UTF-8"))
     :cljs (.-length (js/Buffer.from s "utf8"))))

(defn- render [items]
  #?(:clj (with-out-str (pprint/pprint (vec items)))
     :cljs (str (pr-str (vec items)) "\n")))

(defn pack
  "Greedily pack `items` into shards whose rendered EDN stays under `budget`.

  Each item is measured ONCE; grouping is then arithmetic on those sizes rather
  than a re-render per candidate (which is quadratic, and at 625 rows is the
  difference between a second and minutes). The per-group render is checked
  against the budget afterwards, and an over-budget group sheds its tail — the
  estimate can only be low by the few bytes of pretty-printer indentation, so
  that correction almost never fires, but 'almost never' is not 'never'.

  Deterministic: items arrive sorted and are never reordered, so the same corpus
  always produces the same shard boundaries and therefore the same CIDs. An item
  too large to fit any budget still gets its own shard — dropping it would lose
  data, and `write!` refuses to publish an over-limit shard instead."
  [items budget]
  (let [sized (mapv (fn [it] [it (utf8-count (render [it]))]) items)
        groups (loop [remaining (seq sized), current [], used 0, acc []]
                 (if-not remaining
                   (if (seq current) (conj acc current) acc)
                   (let [[it n] (first remaining)]
                     (if (and (seq current) (> (+ used n) budget))
                       ;; close `current` INTO acc before starting the next group —
                       ;; dropping it here silently loses a shard's worth of rows
                       (recur (next remaining) [it] n (conj acc current))
                       (recur (next remaining) (conj current it) (+ used n) acc)))))]
    (loop [gs (seq groups), out []]
      (if-not gs
        out
        (let [g (first gs)
              s (render g)]
          (if (and (> (utf8-count s) budget) (> (count g) 1))
            ;; shed the tail into the next group and re-measure
            (recur (cons (pop g) (cons [(peek g)] (next gs))) out)
            (recur (next gs) (conj out s))))))))

(defn- shard-parts [dir contents]
  (vec (map-indexed
        (fn [i content]
          {:file (str dir "/" (format "%03d" i) ".kotoba.edn")
           :bytes (utf8-count content)
           :cid (cid/cidv1-raw content)
           :content content})
        contents)))

(defn materialize
  "Corpus rows → sharded corpus + datoms artifacts, each shard a single raw block."
  [patents]
  (let [rows (->> patents (map normalize-patent) (sort-by :id) vec)
        datoms (vec (a/datoms (a/analyze {:fields [] :patents (vec patents)})))]
    {:corpus {:parts (shard-parts corpus-dir (pack rows shard-budget)) :rows (count rows)}
     :datoms {:parts (shard-parts datoms-dir (pack datoms shard-budget)) :rows (count datoms)}}))

(defn publish-manifest [mat as-of]
  (let [strip #(select-keys % [:file :bytes :cid])]
    {:actor :hirameki
     :adr "2606212200"
     :published-at as-of
     :scope :public-patent-bibliographic-metadata
     :artifacts (into {} (map (fn [[k v]]
                                [k {:parts (mapv strip (:parts v))
                                    :rows (:rows v)
                                    :bytes (reduce + 0 (map :bytes (:parts v)))
                                    :shards (count (:parts v))}]))
                      mat)
     ;; A false here means a shard would NOT verify against `ipfs add`, and the
     ;; publish is a bug, not a bigger file.
     :single-block (into {} (map (fn [[k v]]
                                   [k (every? #(< (:bytes %) cid/single-block-limit)
                                              (:parts v))]))
                         mat)
     :verify "clojure -M:query verify.clj"
     :canonical-format :edn}))

#?(:clj
   (defn write! [patents data-dir as-of]
     (when-not (seq data-dir)
       (throw (ex-info "dataset repository path required via argument or HIRAMEKI_DATASET_REPO" {})))
     (let [mat (materialize patents)
           dir (io/file data-dir)
           man (publish-manifest mat as-of)]
       (when-not (every? true? (vals (:single-block man)))
         (throw (ex-info "a shard exceeded the single-block limit — its CID would not match `ipfs add`"
                         {:single-block (:single-block man)})))
       ;; Remove stale shards: a corpus that shrank must not leave orphans behind
       ;; that the manifest no longer lists but a gateway would still serve.
       (doseq [d [corpus-dir datoms-dir]]
         (let [sub (io/file dir d)]
           (when (.isDirectory sub)
             (doseq [f (.listFiles sub)] (.delete f)))
           (.mkdirs sub)))
       (doseq [[_ {:keys [parts]}] mat
               {:keys [file content]} parts]
         (spit (io/file dir file) content))
       (spit (io/file dir "publish-manifest.edn") (str (pr-str man) "\n"))
       man)))

#?(:clj
   (defn publish!
     "Re-materialize the dataset repo from BOTH row sources.

     The corpus has two origins and they must not overwrite each other:

       • `kotoba/seed.edn` — the curated `:representative` rows. Few, but they
         carry real CPC sections, so they are what field-level analytics run on.
       • the harvest journal in the dataset repo — the `:authoritative` rows
         walked out of the citation graph. Many, but CPC-less.

     `normalize/merge-corpus` dedupes by `:id` and lets an `:authoritative` row
     UPGRADE a `:representative` one of the same id, never the reverse. So
     re-publishing after a harvest can only ever replace a hand-written stand-in
     with the mirrored fact."
     [{:keys [seed-path dataset-repo as-of]
       :or {seed-path "kotoba/seed.edn" as-of "manual"}}]
     (when-not (seq dataset-repo)
       (throw (ex-info "dataset repository path required (--dataset or HIRAMEKI_DATASET_REPO)" {})))
     (let [journal-dir (io/file dataset-repo "80-data" "public")
           harvested (if (.isDirectory journal-dir)
                       (corpus/journal->patents (quad/read-sharded (str journal-dir) "google-patents"))
                       [])
           curated (he/patents seed-path)
           merged (nz/merge-corpus curated harvested)]
       {:manifest (write! merged dataset-repo as-of)
        :curated (count curated)
        :harvested (count harvested)
        :total (count merged)})))

#?(:clj
   (defn -main
     "  clojure -M -m hirameki.methods.dataset --dataset ../hirameki-patents --as-of 2026-08-10"
     [& argv]
     (let [argv (vec argv)
           arg (fn [flag dflt] (let [i (.indexOf argv flag)]
                                 (if (>= i 0) (nth argv (inc i)) dflt)))
           r (publish! {:seed-path (arg "--seed" "kotoba/seed.edn")
                        :dataset-repo (arg "--dataset" (System/getenv "HIRAMEKI_DATASET_REPO"))
                        :as-of (arg "--as-of" "manual")})
           man (:manifest r)]
       (println (str "rows: " (:curated r) " curated + " (:harvested r) " harvested → "
                     (:total r) " after merge"))
       (doseq [kind [:corpus :datoms]]
         (let [{:keys [bytes shards rows]} (get-in man [:artifacts kind])]
           (println (str "  " (name kind) " " rows " items / " bytes " bytes / "
                         shards " shard(s), each a single raw block"))
           (doseq [p (get-in man [:artifacts kind :parts])]
             (println (str "    " (:file p) "  " (:bytes p) " bytes  " (:cid p)))))))))
