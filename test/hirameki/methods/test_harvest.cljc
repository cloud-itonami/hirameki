(ns hirameki.methods.test-harvest
  "The self-growing harvest loop.

  Only the PURE half is tested here — queue discipline, citation growth, the
  migration off the old shape. The effectful `tick!` is not mocked into a fake
  network: a test that asserts a stub returns what the stub was told to return
  proves nothing about Google Patents. The parser is proven against a real page
  in `kotoba-lang/toshokan-patents`, and the live leg is exercised by actually
  running the harvest.

  The property that matters most here is that **nothing grows without bound**.
  The previous model kept every seed ever created and every seed ever worked,
  with `:max-seeds` capping the lifetime total — which measured out to a hard
  stop at ~5,000 patents about five weeks away."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hirameki.methods.harvest :as h]))

(defn- seed [n] {:id (str "s" n) :query (str "US" n)})
(def q3 [(seed 1) (seed 2) (seed 3)])

(deftest queue-discipline
  (testing "work comes off the head"
    (is (= (seed 1) (h/next-seed q3)))
    (is (nil? (h/next-seed []))))
  (testing "a worked seed leaves the queue — this is what bounds it"
    (is (= [(seed 2) (seed 3)] (h/drop-seed q3 (seed 1))))
    (is (= 2 (count (h/drop-seed q3 (seed 1))))))
  (testing "dropping something absent is a no-op, not an error"
    (is (= q3 (h/drop-seed q3 (seed 99)))))
  (testing "a transient failure rotates to the BACK, it does not stay at the head"
    (let [after (h/defer-seed q3 (seed 1))]
      (is (= (seed 2) (h/next-seed after)))
      (is (= (seed 1) (last after)))
      (is (= 3 (count after)))
      (testing "so a permanently failing id cannot block all progress"
        (is (not= (seed 1) (h/next-seed (h/defer-seed after (seed 2)))))))))

(deftest citation-growth-converges-and-stays-bounded
  (let [policy {:max-new-seeds-per-tick 10 :max-seeds 5000}]
    (testing "cited ids become seeds"
      (let [grown (h/grow-seeds q3 ["US9" "US8"] #{} policy "t0")]
        (is (= 5 (count grown)))
        (is (= #{"cited-us9" "cited-us8"} (into #{} (map :id) (drop 3 grown))))))
    (testing "already queued is skipped, case-insensitively"
      (is (= q3 (h/grow-seeds q3 ["US1" "us2" "Us3"] #{} policy "t0"))))
    (testing "already harvested is skipped — the corpus is the record, so the
              list of worked seeds does not have to be kept"
      (is (= q3 (h/grow-seeds q3 ["US9" "US8"] #{"US9" "US8"} policy "t0"))))
    (testing "known dead is skipped, so a dead id cited twice is queued once"
      (is (= q3 (h/grow-seeds q3 ["US404"] #{"US404"} policy "t0"))))
    (testing "growth per tick is bounded"
      (is (= 7 (count (h/grow-seeds q3 (map #(str "US" (+ 100 %)) (range 50)) #{}
                                    {:max-new-seeds-per-tick 4 :max-seeds 5000} "t0")))))
    (testing "max-seeds bounds the OUTSTANDING queue, not the lifetime total"
      (let [full (h/grow-seeds q3 (map #(str "US" (+ 100 %)) (range 50)) #{}
                               {:max-new-seeds-per-tick 40 :max-seeds 5} "t0")]
        (is (= 5 (count full)))
        (testing "and draining it makes room again — the old model never did"
          (let [drained (reduce h/drop-seed full (take 3 full))]
            (is (= 2 (count drained)))
            (is (= 5 (count (h/grow-seeds drained (map #(str "US" (+ 900 %)) (range 10))
                                          #{} {:max-new-seeds-per-tick 40 :max-seeds 5} "t1")))
                "room reopened as the queue drained")))))
    (testing "junk is dropped, not stored"
      (is (= q3 (h/grow-seeds q3 ["" "   " (apply str (repeat 40 "X"))] #{} policy "t0"))))))

(deftest state-keeps-only-what-must-be-remembered
  (testing "a tick advances counters and nothing per-seed"
    (let [s (h/record-tick {:ticks 0} "2026-08-10T00:00:00Z")]
      (is (= 1 (:ticks s)))
      (is (= "2026-08-10T00:00:00Z" (:last-tick s)))
      (is (empty? (:dead s)))))
  (testing "a 404 is remembered forever — it will not exist later either"
    (let [s (-> {:ticks 0}
                (h/record-tick "t1" :dead "US404")
                (h/record-tick "t2" :dead "US405")
                (h/record-tick "t3" :dead "US404"))]
      (is (= #{"US404" "US405"} (:dead s)) "a set, so a re-cited dead id costs nothing")
      (is (= 3 (:ticks s))))))

(deftest migration-off-the-append-only-shape
  (let [seeds {:policy {:max-seeds 5000}
               :seeds [{:id "s1" :query "US1"} {:id "s2" :query "US2"}
                       {:id "s3" :query "US3"} {:id "s4" :query "US4"}]}
        state {:exhausted #{"s1" "s2"} :cursor 2 :ticks 2 :last-tick "t"}
        ;; US1 made it into the corpus; US2 did not, so it was a 404
        harvested #{"US1"}
        {:keys [seeds state] :as out} (h/migrate-state seeds state harvested)]
    (testing "worked seeds leave the queue"
      (is (= ["s3" "s4"] (mapv :id (:seeds seeds))))
      (is (= 5000 (get-in seeds [:policy :max-seeds])) "policy is preserved"))
    (testing "a worked seed absent from the corpus was a 404 and is remembered"
      (is (= #{"US2"} (:dead state)))
      (is (not (contains? (:dead state) "US1")) "harvested ids need no memory"))
    (testing "the unbounded fields are gone"
      (is (nil? (:exhausted state)))
      (is (nil? (:cursor state)))
      (is (nil? (:pages state))))
    (testing "idempotent — a second pass finds nothing left to move"
      (let [again (h/migrate-state seeds state harvested)]
        (is (= (:seeds out) (:seeds again)))
        (is (= (:state out) (:state again)))
        (is (= 2 (:dropped out)))
        (is (= 0 (:dropped again))
            ":dropped is a report about THIS pass, so it is the one field that
             must differ — it says the queue is already clean")))
    (testing "already-migrated state passes through untouched"
      (let [fresh {:policy {} :seeds [(seed 1)]}
            st {:ticks 9 :dead #{"X"}}]
        (is (= fresh (:seeds (h/migrate-state fresh st #{}))))
        (is (= 0 (:dropped (h/migrate-state fresh st #{}))))))
    (testing "a seed already in the corpus is dropped even when :exhausted does
              not name it — the original daemon wrote pair keys, not seed ids,
              which left ~618 harvested seeds queued to be re-fetched"
      (let [odd-state {:exhausted #{"google-patents|s1|p1"} :ticks 2}
            out (h/migrate-state {:policy {} :seeds [(seed 1) (seed 2) (seed 3)]}
                                 odd-state #{"US1" "US2"})]
        (is (= ["s3"] (mapv :id (:seeds (:seeds out)))))
        (is (= 2 (:dropped out)))
        (is (empty? (:dead (:state out)))
            "both were harvested, so neither is dead")))
    (testing "a seed neither worked nor harvested stays queued"
      (let [out (h/migrate-state {:policy {} :seeds [(seed 7)]} {:ticks 0} #{})]
        (is (= ["s7"] (mapv :id (:seeds (:seeds out)))))))))

(deftest seeds-file-is-line-oriented-and-still-one-form
  (let [data {:policy {:max-seeds 5000} :seeds [(seed 1) (seed 2)]}
        text (h/render-edn data)]
    (testing "round-trips through the plain reader — the shape did not change"
      (is (= data (clojure.edn/read-string text))))
    (testing "one seed per line, so a diff shows which seeds moved"
      (is (= 4 (count (str/split-lines text))))
      (is (str/includes? text "\n{:id \"s1\"")))
    (testing "a map with no :seeds is written plainly"
      (is (= {:ticks 3} (clojure.edn/read-string (h/render-edn {:ticks 3})))))))
