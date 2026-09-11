(ns hirameki.methods.test-shard
  "Sharding — the property that matters is CONSERVATION.

  A packer that loses rows still produces a valid-looking manifest: correct
  CIDs, `single-block true`, a plausible byte count. Nothing downstream
  complains, because every shard it did write is internally consistent. The
  first version of `pack` dropped a completed group instead of accumulating it
  and wrote 54 of 625 rows; the manifest cheerfully reported one shard.

  So these tests read shards back and count. Byte totals and CIDs are not
  evidence of completeness."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.edn :as edn]
            [hirameki.methods.dataset :as ds]))

(defn- unpack [shards] (vec (mapcat edn/read-string shards)))

(def rows
  (vec (for [i (range 500)]
         {:id (format "US%07d" i)
          :title (str "A patent about subject " i " and its many applications")
          :jurisdiction :us
          :assignee (keyword (str "holder-" (mod i 37)))
          :filing-year (+ 1990 (mod i 30))
          :status :granted
          :source (str "https://patents.google.com/patent/US" i "/en")})))

(deftest packing-conserves-every-item
  (testing "a budget that forces many shards still round-trips every row"
    (doseq [budget [1000 5000 20000 100000]]
      (let [shards (ds/pack rows budget)]
        (is (= rows (unpack shards))
            (str "rows lost or reordered at budget " budget))
        (is (> (count shards) 1)
            (str "budget " budget " should have forced a split, or this proves nothing")))))
  (testing "a budget larger than the whole corpus produces exactly one shard"
    (let [shards (ds/pack rows 10000000)]
      (is (= 1 (count shards)))
      (is (= rows (unpack shards)))))
  (testing "empty input is empty output, not a shard containing nothing"
    (is (= [] (ds/pack [] 1000)))))

(deftest shards-stay-under-budget
  (doseq [budget [2000 20000]]
    (let [shards (ds/pack rows budget)
          oversize (remove #(<= (count (.getBytes ^String % "UTF-8")) budget) shards)]
      (is (empty? (butlast oversize))
          (str "shards over budget " budget ": " (count oversize))))))

(deftest packing-is-deterministic
  (testing "same rows, same budget → byte-identical shards, therefore same CIDs"
    (is (= (ds/pack rows 20000) (ds/pack rows 20000)))))

(deftest materialize-conserves-and-stays-single-block
  (let [mat (ds/materialize rows)
        man (ds/publish-manifest mat "test")]
    (testing "every shard is inside the raw-block limit — a false here is a bug,
              because the CID would not match `ipfs add` at all"
      (is (every? true? (vals (:single-block man)))))
    (testing "the manifest's item count is the count you get by reading the shards back"
      (is (= (count rows)
             (count (unpack (map :content (get-in mat [:corpus :parts]))))
             (get-in man [:artifacts :corpus :rows]))))
    (testing "declared bytes equal the shards' actual bytes"
      (is (= (get-in man [:artifacts :corpus :bytes])
             (reduce + 0 (map #(count (.getBytes ^String (:content %) "UTF-8"))
                              (get-in mat [:corpus :parts]))))))))
