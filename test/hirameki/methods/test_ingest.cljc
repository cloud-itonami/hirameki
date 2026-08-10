(ns hirameki.methods.test-ingest
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hirameki.methods.ingest :as ing]))

;; A fixture in the documented USPTO ODP `patentFileWrapperDataBag` shape. It deliberately
;; INCLUDES an inventorBag (person names) to prove they are NEVER carried into the corpus (G6).
(def odp-rec
  {"applicationNumberText" "16123456"
   "applicationMetaData"
   {"inventionTitle" "Method of fabricating a gate-all-around transistor"
    "patentNumber" "11999888"
    "filingDate" "2018-09-14"
    "grantDate" "2021-04-06"
    "applicationStatusDescriptionText" "Patented Case"
    "cpcClassificationBag" ["H01L29/0673" "H01L21/8234"]
    "applicantBag" [{"applicantNameText" "Taiwan Semiconductor Manufacturing Co., Ltd."}]
    "inventorBag" [{"inventorNameText" "Jane Q. Public"}
                   {"inventorNameText" "John A. Doe"}]}})

(def odp-pending
  {"applicationNumberText" "17222333"
   "applicationMetaData"
   {"inventionTitle" "Solid-state battery electrolyte"
    "filingDate" "2021-02-01"
    "applicationStatusDescriptionText" "Non Final Action Mailed"
    "cpcClassificationBag" ["H01M10/0562"]
    "applicantBag" [{"applicantNameText" "Toyota Motor Corporation"}]}})

(deftest normalizes-to-authoritative-cited-row
  (let [p (ing/odp->patent odp-rec)]
    (is (= :patent (:type p)))
    (is (= "11999888" (:id p)) "patent number is the id when granted")
    (is (= :us (:jurisdiction p)))
    (is (= "H01L" (:field p)) "field = CPC subclass (first 4 chars)")
    (is (= :granted (:status p)))
    (is (= 2018 (:filing-year p)))
    (is (= 2021 (:grant-year p)))
    (is (= :authoritative (:sourcing p)))
    (is (str/starts-with? (:source p) "https://data.uspto.gov/") "G9: cited source URL")))

(deftest g6-no-inventor-person-leaks
  (let [p (ing/odp->patent odp-rec)
        s (pr-str p)]
    ;; the assignee is the ORG; not a person
    (is (= :taiwan-semiconductor-manufacturing (:assignee p)))
    (is (not (str/includes? s "Jane")) "inventor names never enter the corpus (G6)")
    (is (not (str/includes? s "Doe")))
    (is (not (str/includes? (str (keys p)) ":inventor")))))

(deftest g2-no-imposes-edge
  (let [s (pr-str (ing/odp->patent odp-rec))]
    (is (not (str/includes? s ":imposes")))))

(deftest pending-has-no-grant-year
  (let [p (ing/odp->patent odp-pending)]
    (is (= "17222333" (:id p)) "falls back to application number")
    (is (= :pending (:status p)))
    (is (nil? (:grant-year p)))
    (is (= :toyota-motor (:assignee p)))))

(deftest assignee-normalization-strips-suffixes
  (is (= :taiwan-semiconductor-manufacturing
         (ing/assignee->keyword "Taiwan Semiconductor Manufacturing Co., Ltd.")))
  (is (= :international-business-machines
         (ing/assignee->keyword "International Business Machines Corporation"))))

(deftest merge-upgrades-representative-to-authoritative
  (let [existing [{:type :patent :id "11999888" :title "old" :sourcing :representative}
                  {:type :patent :id "OTHER1" :title "keep" :sourcing :representative}]
        incoming (ing/odp->patents [odp-rec])
        merged (ing/merge-corpus existing incoming)]
    (is (= 2 (count merged)) "dedup by id")
    (let [u (first (filter #(= "11999888" (:id %)) merged))]
      (is (= :authoritative (:sourcing u)) "authoritative replaces representative"))
    (is (some #(= "OTHER1" (:id %)) merged) "untouched representative kept")
    (is (= (sort (map :id merged)) (map :id merged)) "sorted by id")))

(deftest authoritative-not-downgraded-by-representative
  (let [existing [{:type :patent :id "X" :sourcing :authoritative :source "u"}]
        incoming [{:type :patent :id "X" :sourcing :representative}]
        merged (ing/merge-corpus existing incoming)]
    (is (= :authoritative (:sourcing (first merged))) "representative never overwrites authoritative")))
