(ns hirameki.methods.hirameki-edn
  "hirameki 閃き — seed loader + classifier for the public-patent KG-mirror substrate.

  clj-native, dependency-free (clojure.edn). Each actor reads its own substrate; sibling
  of busshi/kabuto/kanjō/tokigusuri loaders. The seed mixes two node kinds — :field (the
  concentration unit) and :patent (exemplar corpus rows) — so the loader classifies them
  apart. ADR-2606212200.

  kotoba/seed.edn is stored on disk as Datomic/Datascript tx-data (a vector of
  entity maps, each with :db/id and namespaced attrs like :hirameki.field/id —
  Phase 4 datomize pass). `load-edn`/`parse-edn` reconstitute each entity back
  into a bare :type/:id/... map (stripping the namespace, unblobbing pr-str'd
  nested values) so every downstream consumer (analyze/dataset/ingest/autorun
  + tests) keeps working against the original bare-key shape unchanged."
  (:require [clojure.edn :as edn]
            #?(:clj [clojure.java.io :as io])))

(defn- unblob
  "Non-scalar attr values were pr-str'd into a blob string by the datomize pass;
  undo that. Leaves ordinary (non-blob) string values untouched — only replaces
  v when it round-trips through edn/read-string as a collection."
  [v]
  (if (string? v)
    (try (let [parsed (edn/read-string v)] (if (coll? parsed) parsed v))
         (catch #?(:clj Exception :cljs :default) _ v))
    v))

(defn- tx-data?
  "Detect the wrapped Datomic/Datascript tx-data shape (a vector of entity maps,
  each carrying :db/id) vs. the original bare-map-per-row shape."
  [content]
  (and (vector? content) (seq content) (map? (first content)) (contains? (first content) :db/id)))

(defn- reconstitute-row
  "Strip an entity's namespace back to bare keys (:hirameki.field/id -> :id) and
  drop :db/id, undoing wrap-map!/entity-from-map."
  [row]
  (into {} (map (fn [[k v]] [(keyword (name k)) (unblob v)])) (dissoc row :db/id)))

(defn- reconstitute [content]
  (if (tx-data? content) (mapv reconstitute-row content) content))

(defn parse-edn
  "Parse an EDN string (a top-level vector of node maps, tx-data or bare)."
  [text]
  (reconstitute (edn/read-string text)))

(defn load-edn
  "Read an EDN seed file into Clojure data. :clj file I/O only."
  [path]
  #?(:clj (reconstitute
           (with-open [r (java.io.PushbackReader. (io/reader path))]
             (edn/read r)))
     :cljs (throw (ex-info "load-edn is :clj-only" {:path path}))))

(defn classify
  "Split seed rows by :type into {:fields [...] :patents [...]}."
  [rows]
  {:fields  (vec (filter #(= (:type %) :field) rows))
   :patents (vec (filter #(= (:type %) :patent) rows))})

(defn fields
  "Convenience: load + return only the :field rows. :clj-only."
  [path]
  (:fields (classify (load-edn path))))

(defn patents
  "Convenience: load + return only the :patent rows. :clj-only."
  [path]
  (:patents (classify (load-edn path))))
