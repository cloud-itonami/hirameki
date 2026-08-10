(ns hirameki.methods.test-social
  (:require [clojure.test :refer [deftest is]]
            [hirameki.methods.social :as social]))

(deftest publication-adapter-is-repository-native
  (is (fn? social/draft-observation-post))
  (is (fn? social/build-live))
  (is (string? social/DISCLAIMER)))
