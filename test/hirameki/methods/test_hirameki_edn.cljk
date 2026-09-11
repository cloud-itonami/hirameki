(ns hirameki.methods.test-hirameki-edn
  (:require [clojure.test :refer [deftest is]]
            [hirameki.methods.hirameki-edn :as he]))

(def seed "kotoba/seed.edn")

(deftest parse-edn-roundtrips
  (is (= [{:type :field :id "X"}] (he/parse-edn "[{:type :field :id \"X\"}]"))))

(deftest load-and-classify
  (let [{:keys [fields patents]} (he/classify (he/load-edn seed))]
    (is (pos? (count fields)) "seed has fields")
    (is (pos? (count patents)) "seed has patents")
    (is (every? #(= :field (:type %)) fields))
    (is (every? #(= :patent (:type %)) patents))))

(deftest convenience-loaders
  (is (every? #(= :field (:type %)) (he/fields seed)))
  (is (every? #(= :patent (:type %)) (he/patents seed))))
