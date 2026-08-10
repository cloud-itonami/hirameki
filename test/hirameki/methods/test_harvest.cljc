(ns hirameki.methods.test-harvest
  "The self-growing harvest loop.

  Only the PURE half is tested here — seed selection, citation-graph growth,
  cursor advance. The effectful `tick!` is not mocked into a fake network:
  a test that asserts a stub returns what the stub was told to return proves
  nothing about Google Patents. The parser is proven against a real page in
  `kotoba-lang/toshokan-patents`, and the live leg is exercised by actually
  running the harvest."
  (:require [clojure.test :refer [deftest is testing]]
            [hirameki.methods.harvest :as h]))

(def seeds
  [{:id "a" :query "US1"}
   {:id "b" :query "US2"}
   {:id "c" :query "US3"}])

(deftest seed-selection-is-round-robin-and-skips-exhausted
  (testing "a fresh cursor takes the first seed"
    (is (= "a" (get-in (h/next-seed {:cursor 0} seeds) [:seed :id]))))
  (testing "the cursor wraps"
    (is (= "a" (get-in (h/next-seed {:cursor 3} seeds) [:seed :id])))
    (is (= "b" (get-in (h/next-seed {:cursor 4} seeds) [:seed :id]))))
  (testing "exhausted seeds are skipped, not re-fetched"
    (is (= "c" (get-in (h/next-seed {:cursor 0 :exhausted #{"a" "b"}} seeds) [:seed :id]))))
  (testing "everything exhausted → nil, so the loop reports rather than spins"
    (is (nil? (h/next-seed {:cursor 0 :exhausted #{"a" "b" "c"}} seeds))))
  (testing "an empty seed list is nil, not an exception"
    (is (nil? (h/next-seed {:cursor 0} [])))))

(deftest citation-growth-converges
  (let [policy {:max-new-seeds-per-tick 10 :max-seeds 5000}]
    (testing "cited ids become seeds"
      (let [grown (h/grow-seeds seeds ["US9" "US8"] policy "t0")]
        (is (= 5 (count grown)))
        (is (= #{"cited-us9" "cited-us8"}
               (into #{} (map :id) (drop 3 grown))))
        (is (every? #(= "google-patents" (:grown-from %)) (drop 3 grown)))))
    (testing "already-known queries are not re-added — this is what makes it converge"
      (is (= seeds (h/grow-seeds seeds ["US1" "US2" "US3"] policy "t0")))
      (is (= seeds (h/grow-seeds seeds ["us1" "Us2"] policy "t0"))
          "case-insensitively known"))
    (testing "growth per tick is bounded"
      (let [grown (h/grow-seeds seeds (map #(str "US" (+ 100 %)) (range 50))
                                {:max-new-seeds-per-tick 4 :max-seeds 5000} "t0")]
        (is (= 7 (count grown)))))
    (testing "the frontier is bounded overall, so a citation graph cannot run away"
      (let [grown (h/grow-seeds seeds (map #(str "US" (+ 100 %)) (range 50))
                                {:max-new-seeds-per-tick 40 :max-seeds 5} "t0")]
        (is (= 5 (count grown)))))
    (testing "junk is dropped, not stored"
      (is (= seeds (h/grow-seeds seeds ["" "   " (apply str (repeat 40 "X"))] policy "t0"))))))

(deftest advance-marks-exhausted-and-moves-on
  (let [s (h/advance {:cursor 0 :ticks 0} 0 "a" "2026-08-10T00:00:00Z")]
    (is (= 1 (:cursor s)))
    (is (= 1 (:ticks s)))
    (is (= #{"a"} (:exhausted s)))
    (is (= "2026-08-10T00:00:00Z" (:last-tick s))))
  (testing "advancing repeatedly accumulates rather than replacing"
    (let [s (-> {:cursor 0 :ticks 0}
                (h/advance 0 "a" "t1")
                (h/advance 1 "b" "t2"))]
      (is (= #{"a" "b"} (:exhausted s)))
      (is (= 2 (:ticks s)))))
  (testing "one lookup exhausts its seed — a patent id resolves to at most one page"
    (let [s (h/advance {:cursor 0} 0 "a" "t")]
      (is (nil? (h/next-seed s [(first seeds)]))))))
