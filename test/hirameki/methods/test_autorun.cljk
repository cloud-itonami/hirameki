(ns hirameki.methods.test-autorun
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.io :as io]
            [hirameki.methods.hirameki-edn :as he]
            [hirameki.methods.kotoba :as k]
            [hirameki.methods.autorun :as ar]))

(def seed "kotoba/seed.edn")
(def tmp "target/test-autorun.kotoba.edn")
(def rows (he/classify (he/load-edn seed)))

(defn- clean! [] (let [f (io/file tmp)] (when (.exists f) (.delete f))))

(deftest beat-persists-and-verifies
  (clean!)
  (let [r (ar/beat {:seed-rows rows :ref-year 2026 :tx-id "b0" :as-of "as0" :log-path tmp})]
    (is (:appended r))
    (is (pos? (:count r)))
    (is (pos? (:fields r)))
    (is (pos? (:patents r)))
    (is (:ok (k/verify-chain tmp)))
    (clean!)))

(deftest second-identical-beat-is-noop
  (clean!)
  (let [r0 (ar/beat {:seed-rows rows :ref-year 2026 :tx-id "b0" :as-of "as0" :log-path tmp})
        r1 (ar/beat {:seed-rows rows :ref-year 2026 :tx-id "b1" :as-of "as1" :log-path tmp})]
    (is (:appended r0))
    (is (not (:appended r1)) "identical observation datoms → no-op")
    (is (= :no-change (:reason r1)))
    (is (= 1 (count (k/read-log tmp))) "ledger did not grow")
    (clean!)))

(deftest changed-observations-append
  (clean!)
  (let [r0 (ar/beat {:seed-rows rows :ref-year 2026 :tx-id "b0" :as-of "as0" :log-path tmp})
        ;; advance the clock — release-status of expiring-soon patents changes
        r1 (ar/beat {:seed-rows rows :ref-year 2030 :tx-id "b1" :as-of "as1" :log-path tmp})]
    (is (:appended r0))
    (is (:appended r1) "different ref-year → different release-status → appends")
    (is (= 2 (count (k/read-log tmp))))
    (is (:ok (k/verify-chain tmp)))
    (clean!)))

(deftest beat-deterministic
  (clean!)
  (let [c1 (:head (ar/beat {:seed-rows rows :ref-year 2026 :tx-id "x" :as-of "y" :log-path tmp}))]
    (clean!)
    (let [c2 (:head (ar/beat {:seed-rows rows :ref-year 2026 :tx-id "x" :as-of "y" :log-path tmp}))]
      (is (= c1 c2) "same inputs → same head CID")
      (clean!))))
