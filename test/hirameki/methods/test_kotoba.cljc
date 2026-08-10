(ns hirameki.methods.test-kotoba
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [hirameki.methods.kotoba :as k]))

(def tmp "target/test-ledger.kotoba.edn")

(defn- clean! [] (let [f (io/file tmp)] (when (.exists f) (.delete f))))

(deftest tx-cid-deterministic-and-content-sensitive
  (let [d1 [[:db/add "e" :a 1]]
        d2 [[:db/add "e" :a 2]]]
    (is (= (k/tx-cid d1 "") (k/tx-cid d1 "")))
    (is (not= (k/tx-cid d1 "") (k/tx-cid d2 "")))
    (is (not= (k/tx-cid d1 "") (k/tx-cid d1 "prev")) "prev-cid changes the CID")))

(deftest append-read-roundtrip
  (clean!)
  (let [tx (k/make-tx [[:db/add "e" :a 1]] "t0" "as0" "")
        cid (k/append-tx tx tmp)
        log (k/read-log tmp)]
    (is (= cid (:tx/cid tx)))
    (is (= 1 (count log)))
    (is (= cid (k/head-cid tmp)))
    (clean!)))

(deftest chaining-and-verify
  (clean!)
  (let [t0 (k/make-tx [[:db/add "e" :a 1]] "t0" "as0" "")
        c0 (k/append-tx t0 tmp)
        t1 (k/make-tx [[:db/add "e" :a 2]] "t1" "as1" c0)
        _  (k/append-tx t1 tmp)
        v  (k/verify-chain tmp)]
    (is (:ok v))
    (is (= 2 (:length v)))
    (is (= -1 (:broken-at v)))
    (clean!)))

(deftest tamper-detected
  (clean!)
  (let [t0 (k/make-tx [[:db/add "e" :a 1]] "t0" "as0" "")
        _  (k/append-tx t0 tmp)]
    ;; corrupt the datoms but keep the (now-stale) cid
    (spit tmp (clojure.string/replace (slurp tmp) "\"e\"" "\"TAMPERED\""))
    (is (not (:ok (k/verify-chain tmp))))
    (clean!)))
