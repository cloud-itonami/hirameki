(ns hirameki.methods.test-corpus
  "The corpus boundary — where harvested journal quads become hirameki rows.

  The tests that matter here are the ones about what is DROPPED. A harvester
  stores what the page published; the actor must not inherit all of it."
  (:require [clojure.test :refer [deftest is testing]]
            [hirameki.methods.corpus :as c]
            [hirameki.methods.analyze :as a]))

;; A two-patent journal in exactly the shape the harvest leg writes, INCLUDING
;; the inventor quads that gate G6 has to remove.
(def journal
  [["gp:US8697359B1" :patent/source :google-patents 1 :add]
   ["gp:US8697359B1" :patent/patent-id "US8697359B1" 1 :add]
   ["gp:US8697359B1" :patent/number "US:8697359" 1 :add]
   ["gp:US8697359B1" :patent/country "US" 1 :add]
   ["gp:US8697359B1" :patent/title "CRISPR-Cas systems" 1 :add]
   ["gp:US8697359B1" :patent/filed-at "2013-10-15" 1 :add]
   ["gp:US8697359B1" :patent/granted-at "2014-04-15" 1 :add]
   ["gp:US8697359B1" :patent/inventor "Feng Zhang" 1 :add]
   ["gp:US8697359B1" :patent/applicant "Broad Institute Inc" 1 :add]
   ["gp:US8697359B1" :patent/applicant "Massachusetts Institute of Technology" 1 :add]
   ["gp:US8697359B1" :patent/cites "US4683202" 1 :add]
   ["gp:US8697359B1" :patent/cites "US5143854" 1 :add]
   ["gp:US8697359B1" :patent/source-url "https://patents.google.com/patent/US8697359B1/en" 1 :add]
   ["gp:JP2004224907A" :patent/source :google-patents 2 :add]
   ["gp:JP2004224907A" :patent/patent-id "JP2004224907A" 2 :add]
   ["gp:JP2004224907A" :patent/country "JP" 2 :add]
   ["gp:JP2004224907A" :patent/title "Coating composition" 2 :add]
   ["gp:JP2004224907A" :patent/filed-at "2003-01-24" 2 :add]
   ["gp:JP2004224907A" :patent/inventor "個人名" 2 :add]
   ["gp:JP2004224907A" :patent/applicant "Kansai Paint Co Ltd" 2 :add]
   ["gp:JP2004224907A" :patent/cites "US8697359B1" 2 :add]
   ["gp:JP2004224907A" :patent/source-url "https://patents.google.com/patent/JP2004224907A/en" 2 :add]])

(def rows (c/journal->patents journal))

(deftest g6-drops-person-level-data
  (testing "the journal carries inventors; a hirameki row never does"
    (is (some #(= :patent/inventor (nth % 1)) journal)
        "fixture must actually contain the thing being dropped, or this proves nothing")
    (is (every? #(nil? (:inventor %)) rows))
    (is (every? #(nil? (:inventors %)) rows))
    (let [all-values (mapcat vals rows)]
      (is (not-any? #{"Feng Zhang" "個人名"} all-values)
          "no inventor name survives into any field of any row")))
  (testing "the assignee is the ORG, normalized"
    (is (= :broad-institute (:assignee (first (filter #(= "US8697359B1" (:id %)) rows)))))
    (is (= :kansai-paint (:assignee (first (filter #(= "JP2004224907A" (:id %)) rows))))))
  (testing "co-assignment is kept in full; the singular is a projection of it"
    (let [p (first (filter #(= "US8697359B1" (:id %)) rows))]
      (is (= [:broad-institute :massachusetts-institute-of-technology] (:assignees p)))
      (is (= (first (:assignees p)) (:assignee p))))))

;; a/add emits [":db/add" entity attr value] — index 2 is the attribute.
(def summary-ds (c/summary-datoms (c/summarize rows 2026)))
(def summary-attrs (into #{} (map #(nth % 2)) summary-ds))

(deftest g2-a-patent-never-imposes
  (testing "no emitted attribute makes a patent a holder"
    (is (seq summary-attrs) "fixture must emit something, or this proves nothing")
    (is (not-any? #(re-find #"imposes" (str %)) summary-attrs))))

(deftest g1-g3-no-verdicts-emitted
  (testing "no infringement / FTO / equity attribute is representable in the emission"
    (is (not-any? #(re-find #"infringement|fto|equity-signal" (str %)) summary-attrs)))
  (testing "every emitted datom is a well-formed [:db/add e a v]"
    (is (every? #(= 4 (count %)) summary-ds))
    (is (every? #(= ":db/add" (first %)) summary-ds))))

(deftest rows-are-faithful-and-deterministic
  (testing "sorted by id, so a re-fold is byte-identical"
    (is (= (map :id rows) (sort (map :id rows))))
    (is (= rows (c/journal->patents journal)))
    (is (= rows (c/journal->patents (shuffle journal)))
        "quad order must not change the result"))
  (testing "bibliographic facts are mirrored, not re-judged"
    (let [p (first (filter #(= "US8697359B1" (:id %)) rows))]
      (is (= :us (:jurisdiction p)))
      (is (= 2013 (:filing-year p)))
      (is (= 2014 (:grant-year p)))
      (is (= :granted (:status p)))
      (is (= :authoritative (:sourcing p)))
      (is (= ["US4683202" "US5143854"] (:cites p)))))
  (testing "a patent with no grant date is pending, not guessed granted"
    (is (= :pending (:status (first (filter #(= "JP2004224907A" (:id %)) rows))))))
  (testing "CPC is absent from the source, so the field is UNKNOWN — never invented"
    (is (every? #(= "UNKNOWN" (:field %)) rows))))

(deftest release-clock-reads-the-corpus
  (let [d (c/release-distribution rows 2026)]
    (is (= 2 (reduce + (vals d))))
    (is (contains? d "in-force")))
  (testing "advancing the reference year moves patents toward the commons"
    (let [now (c/release-distribution rows 2026)
          later (c/release-distribution rows 2040)]
      (is (not= now later))
      (is (pos? (get later "public-domain" 0))))))

(deftest concentration-is-a-lower-bound
  (let [conc (c/assignee-concentration rows)]
    (is (= 2 (get conc "corpus_size")))
    (is (= 2 (get conc "distinct_assignees")))
    (is (= 0 (get conc "unassigned")))
    (is (<= 0.0 (get conc "named_hhi") 1.0)))
  (testing "unassigned rows are excluded from the share base, like :other is"
    (let [with-unassigned (conj rows {:type :patent :id "ZZ1" :assignee :unassigned
                                      :field "UNKNOWN" :status :pending})
          conc (c/assignee-concentration with-unassigned)]
      (is (= 3 (get conc "corpus_size")))
      (is (= 2 (get conc "named")))
      (is (= 1 (get conc "unassigned"))))))

(deftest citation-frontier-is-honest
  (let [s (c/citation-stats rows)]
    (is (= 3 (get s "edges")))
    (is (= 3 (get s "distinct_cited")))
    (testing "US8697359B1 is cited AND harvested; the other two are frontier"
      (is (= 1 (get s "cited_already_harvested")))
      (is (= 2 (get s "frontier"))))))

(deftest hhi-is-computed-and-rounded-like-the-field-analytics
  (testing "two assignees at 50% each → HHI 0.5, the same formula analyze/named-hhi uses"
    (is (= 0.5 (get (c/assignee-concentration rows) "named_hhi"))))
  (testing "shares and HHI are rounded to 3 decimals, as everywhere else"
    (let [conc (c/assignee-concentration
                (into rows [{:assignee :a} {:assignee :a} {:assignee :b}]))]
      (is (= (a/round3 (get conc "named_hhi")) (get conc "named_hhi")))
      (is (every? #(= (a/round3 (get % "share")) (get % "share"))
                  (get conc "top"))))))
