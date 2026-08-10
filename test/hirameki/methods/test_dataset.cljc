(ns hirameki.methods.test-dataset
  "Dataset materialization, updated 2026-08-10 for sharded artifacts.

  The single-file `corpus-edn` / `datoms-edn` shape is gone: at 625 rows the
  corpus outgrew the 256 KiB raw-block limit, and a `bafkrei…` CID stops being
  what `ipfs add` produces at exactly that point. The gate assertions below now
  run over the concatenated shard text, so they check the same bytes a consumer
  would actually fetch."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hirameki.methods.hirameki-edn :as he]
            [hirameki.methods.cid :as cid]
            [hirameki.methods.dataset :as ds]))

(def patents (he/patents "kotoba/seed.edn"))

(defn- text
  "All shards of one artifact kind, concatenated — what a consumer ends up with."
  [mat kind]
  (str/join "\n" (map :content (get-in mat [kind :parts]))))

(deftest materialize-deterministic-and-content-addressed
  (let [m1 (ds/materialize patents)
        m2 (ds/materialize patents)]
    (is (= m1 m2) "same corpus → byte-identical artifacts → same CIDs (idempotent)")
    (doseq [kind [:corpus :datoms]
            part (get-in m1 [kind :parts])]
      (is (str/starts-with? (:cid part) "bafkrei")
          "raw single-block CID, not a dag-pb root")
      (is (= (:cid part) (cid/cidv1-raw (:content part)))
          "manifest CID = content-address of the bytes it names")
      (is (< (:bytes part) cid/single-block-limit)
          "a shard past the limit would not verify against `ipfs add` at all"))))

(deftest corpus-is-sorted-and-normalized
  (let [ids (->> (get-in (ds/materialize patents) [:corpus :parts])
                 (mapcat #(read-string (:content %)))
                 (map :id))]
    (is (= ids (sort ids)) "corpus rows sorted by id, across shard boundaries too")))

(deftest g2-corpus-has-no-imposes
  ;; a patent record is the gated object — it never carries an imposes/holder edge
  (let [c (text (ds/materialize patents) :corpus)]
    (is (not (str/includes? c ":imposes")))
    (is (not (str/includes? c "imposes-on")))))

(deftest g6-no-inventor-person-field
  (let [c (text (ds/materialize patents) :corpus)]
    (is (not (str/includes? c ":inventor")))
    (is (not (str/includes? c ":person")))))

(deftest manifest-shape
  (let [man (ds/publish-manifest (ds/materialize patents) "2026-06-21T00:00:00Z")]
    (is (= :hirameki (:actor man)))
    (is (= "2606212200" (:adr man)))
    (is (= :public-patent-bibliographic-metadata (:scope man)))
    (testing "each artifact declares its parts, row count and total bytes"
      (is (seq (get-in man [:artifacts :corpus :parts])))
      (is (= (count patents) (get-in man [:artifacts :corpus :rows])))
      (is (pos? (get-in man [:artifacts :corpus :bytes])))
      (is (= (count (get-in man [:artifacts :corpus :parts]))
             (get-in man [:artifacts :corpus :shards]))))
    (testing "every part carries file, bytes and cid — and nothing else"
      (is (every? #(= #{:file :bytes :cid} (set (keys %)))
                  (get-in man [:artifacts :corpus :parts]))))
    (is (true? (get-in man [:single-block :corpus])))))
