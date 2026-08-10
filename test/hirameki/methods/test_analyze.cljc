(ns hirameki.methods.test-analyze
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hirameki.methods.hirameki-edn :as he]
            [hirameki.methods.analyze :as a]))

(def seed "kotoba/seed.edn")
(def rows (he/classify (he/load-edn seed)))
(def analysis (a/analyze rows 2026))

;; ── concentration ────────────────────────────────────────────────────────────
(deftest top-share-excludes-other
  (let [f {:assignees [[:a 16] [:b 14] [:other 70]]}]
    (is (= 16 (a/top-assignee-share f)) ":other never counts as the top holder")
    (is (= :a (a/top-assignee f)))))

(deftest hhi-is-lower-bound
  (let [f {:assignees [[:a 16] [:b 14] [:other 70]]}]
    ;; (16² + 14²)/10000 = (256+196)/10000
    (is (< (Math/abs (- (a/named-hhi f) 0.0452)) 1e-9))))

(deftest chokepoint-levels
  (is (= :critical (a/chokepoint-risk 60)))
  (is (= :high (a/chokepoint-risk 40)))
  (is (= :moderate (a/chokepoint-risk 25)))
  (is (= :low (a/chokepoint-risk 18))))

;; ── routing (dominant-driver) ────────────────────────────────────────────────
(deftest route-by-dominant-driver
  ;; entrenched essential chokepoint → de-monopolization
  (is (= :de-monopolization
         (a/route {:assignees [[:a 65] [:other 35]] :essentiality 0.9
                   :expired-share 0.1 :expiring-soon-share 0.0 :open-license-share 0.0})))
  ;; majority commons-bound → release
  (is (= :release
         (a/route {:assignees [[:a 8] [:other 92]] :essentiality 0.5
                   :expired-share 0.5 :expiring-soon-share 0.2 :open-license-share 0.05})))
  ;; pledgeable opening culture → open-license
  (is (= :open-license
         (a/route {:assignees [[:a 9] [:other 91]] :essentiality 0.6
                   :expired-share 0.1 :expiring-soon-share 0.05 :open-license-share 0.25})))
  ;; early-life diversified → monitor
  (is (= :monitor
         (a/route {:assignees [[:a 6] [:other 94]] :essentiality 0.4
                   :expired-share 0.05 :expiring-soon-share 0.02 :open-license-share 0.02}))))

(deftest load-and-readiness-bounded
  (doseq [f (:fields rows)]
    (is (<= 0.0 (a/exclusivity-load f) 1.0))
    (is (<= 0.0 (a/release-readiness f) 1.0))))

(deftest seed-produces-each-route
  (let [routes (set (map #(get % "route") (get analysis "fields")))]
    (is (contains? routes :release))
    (is (contains? routes :open-license))
    (is (contains? routes :monitor))))

;; ── patent release clock ─────────────────────────────────────────────────────
(deftest release-clock
  (is (= :pending (a/release-status {:status :pending} 2026)))
  (is (= :public-domain (a/release-status {:status :expired} 2026)))
  (is (= :public-domain (a/release-status {:status :granted :filing-year 2000 :term-years 20} 2026))
      "term elapsed → public domain even if status stale")
  (is (= :expiring-soon (a/release-status {:status :granted :filing-year 2007 :term-years 20} 2026))
      "expiry 2027, 1 yr left")
  (is (= :in-force (a/release-status {:status :granted :filing-year 2017 :term-years 20} 2026))))

;; ── datom emission shape + invariants ────────────────────────────────────────
(deftest datoms-flagged-derived-and-sourced
  (let [ds (a/datoms analysis)]
    (is (every? #(and (vector? %) (= 4 (count %)) (= ":db/add" (first %))) ds))
    (is (some #(= ":hirameki/derived" (nth % 2)) ds))
    (is (some #(= ":hirameki/sourcing" (nth % 2)) ds))
    (is (some #(= ":hirameki.obs/route" (nth % 2)) ds))
    (is (some #(= ":hirameki.obs/release-status" (nth % 2)) ds))))

(deftest g1-g3-no-verdict-no-signal-no-forecast
  (let [attrs (set (map #(nth % 2) (a/datoms analysis)))]
    (is (not (contains? attrs ":hirameki/infringement-verdict")))
    (is (not (contains? attrs ":hirameki/fto-opinion")))
    (is (not (contains? attrs ":hirameki/equity-signal")))
    (is (not (some #(str/includes? % "forecast") attrs)))))

(deftest g2-patent-never-a-holder
  (let [ds (a/datoms analysis)
        attrs (set (map #(nth % 2) ds))]
    (is (not (contains? attrs ":hirameki.patent/imposes-on"))
        "a patent never imposes — G2")
    ;; every top-assignee is an org keyword string, never a patent entity id
    (doseq [d ds :when (= ":hirameki.obs/top-assignee" (nth d 2))]
      (is (not (str/starts-with? (str (nth d 3)) "hirameki-patent:"))
          "the holder of exclusivity is an assignee org, never a patent"))))

(deftest g6-no-person-inventor
  (let [attrs (set (map #(nth % 2) (a/datoms analysis)))]
    (is (not (contains? attrs ":hirameki.inventor/person")))))

;; ── report is a release map, not a target list ──────────────────────────────
(deftest report-is-release-map-not-target-list
  (let [rep (a/render-report analysis (a/coverage analysis))]
    (is (str/includes? rep "RELEASE map"))
    (is (str/includes? rep "NOT a target-list"))
    (is (str/includes? rep "NOT an"))           ;; "NOT an infringement/FTO/patentability verdict"
    (is (not (str/includes? (str/lower-case rep) "infringe this")))))

(deftest coverage-nonneg
  (let [c (a/coverage analysis)]
    (is (>= (get c "sections_gap") 0))
    (is (pos? (get c "corpus_have")))
    (is (= 200000000 (get c "corpus_world_est")))))
