(ns hirameki.methods.test-normalize
  "Shared normalizers. Every row source folds through these, so a disagreement
  here shows up as one organization counted twice under two keys."
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [hirameki.methods.normalize :as nz]))

(deftest a-key-is-always-readable-back
  (testing "measured 2026-08-12: `(AEC)` in an assignee name opened a list inside
            a map literal and made the whole 504-record shard unparseable"
    (is (= :us-atomic-energy-commission-aec
           (nz/assignee->keyword "US Atomic Energy Commission (AEC)"))))
  (testing "every delimiter that could end a keyword becomes a hyphen"
    (doseq [name ["Acme (Holdings)" "Acme [Group]" "Acme {Labs}" "Acme \"Quoted\""
                  "Acme; Co" "Acme's Lab" "Acme@Work" "Acme/Bio" "Acme#1"
                  "Acme`Tick" "Acme~Tilde" "Acme^Hat" "Acme\\Back"]]
      (let [k (nz/assignee->keyword name)
            round-trip (edn/read-string (pr-str {k 1}))]
        (is (= {k 1} round-trip)
            (str name " → " k " must read back as EDN")))))
  (testing "a run of them collapses instead of leaving `--` or a trailing dash"
    (is (= :acme-labs (nz/assignee->keyword "Acme (Labs)")))
    (is (= :acme (nz/assignee->keyword "(Acme)"))))
  (testing "non-ASCII names survive — an [a-z0-9] allow list would erase them
            into :unassigned, which is a measurement error, not a reader fix"
    (is (= :日本ペイント (nz/assignee->keyword "日本ペイント")))))

(deftest legal-form-variants-are-one-holder
  (testing "the same company written four ways is one key"
    (is (apply = (map nz/assignee->keyword
                      ["Kansai Paint Co., Ltd."
                       "Kansai Paint Co Ltd"
                       "KANSAI PAINT CO., LTD"
                       "  kansai paint co ltd  "]))))
  (is (= :kansai-paint (nz/assignee->keyword "Kansai Paint Co., Ltd.")))
  (is (= :broad-institute (nz/assignee->keyword "Broad Institute Inc"))))

(deftest placeholders-are-not-holders
  (testing "`Individual` names the ABSENCE of an org — measured at 6.0% of the
            618-patent harvest, it would have ranked second among 'holders'"
    (is (nil? (nz/assignee->keyword "Individual")))
    (is (nil? (nz/assignee->keyword "INDIVIDUAL")))
    (is (nil? (nz/assignee->keyword "Individuals"))))
  (testing "other empty-slot markers too"
    (is (nil? (nz/assignee->keyword "N/A")))
    (is (nil? (nz/assignee->keyword "Unknown")))
    (is (nil? (nz/assignee->keyword "Unassigned"))))
  (testing "blank and nil are nil, not a keyword named \"\""
    (is (nil? (nz/assignee->keyword nil)))
    (is (nil? (nz/assignee->keyword "")))
    (is (nil? (nz/assignee->keyword "   ")))
    (is (nil? (nz/assignee->keyword "Inc."))
        "a bare legal suffix reduces to nothing and must not become a holder"))
  (testing "a real org whose name merely CONTAINS a placeholder word survives"
    (is (= :individual-health-systems
           (nz/assignee->keyword "Individual Health Systems Inc")))))

(deftest year-extraction
  (is (= 2013 (nz/year-of "2013-10-15")))
  (is (= 2013 (nz/year-of "2013")))
  (is (nil? (nz/year-of nil)))
  (is (nil? (nz/year-of "not a date"))))

(deftest cpc-decomposition
  (is (= :C (nz/cpc->section "C12N15/11")))
  (is (= "C12N" (nz/cpc->subclass "C12N15/11")))
  (is (nil? (nz/cpc->section nil)))
  (is (nil? (nz/cpc->subclass "C12"))))

(deftest merge-upgrades-representative-to-authoritative
  (let [rep {:id "US1" :title "guess" :sourcing :representative}
        auth {:id "US1" :title "mirrored" :sourcing :authoritative}]
    (testing "an authoritative row replaces a representative one"
      (is (= [auth] (nz/merge-corpus [rep] [auth]))))
    (testing "a representative row NEVER replaces an authoritative one"
      (is (= [auth] (nz/merge-corpus [auth] [rep]))))
    (testing "two authoritative rows keep the incoming one"
      (let [auth2 (assoc auth :title "newer")]
        (is (= [auth2] (nz/merge-corpus [auth] [auth2])))))
    (testing "result is sorted by id regardless of input order"
      (is (= ["US1" "US2" "US3"]
             (map :id (nz/merge-corpus [{:id "US3"} {:id "US1"}] [{:id "US2"}])))))))
