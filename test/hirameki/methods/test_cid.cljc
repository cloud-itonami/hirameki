(ns hirameki.methods.test-cid
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [hirameki.methods.cid :as cid]))

(deftest empty-file-known-vector
  ;; `printf '' | ipfs add --cid-version=1 --raw-leaves` — canonical empty-block CID
  (is (= "bafkreihdwdcefgh4dqkjv67uzcmw7ojee6xedzdetojuzjevtenxquvyku"
         (cid/cidv1-raw ""))))

(deftest hello-known-vector
  ;; `printf 'hello\n' | ipfs add --cid-version=1 --raw-leaves` (verified vs rasen cid.py)
  (is (= "bafkreicysg23kiwv34eg2d7qweipxwosdo2py4ldv42nbauguluen5v6am"
         (cid/cidv1-raw "hello\n"))))

(deftest prefix-and-determinism
  (let [c (cid/cidv1-raw "some patent corpus bytes")]
    (is (str/starts-with? c "bafkrei") "CIDv1 raw/sha2-256 base32 prefix")
    (is (= c (cid/cidv1-raw "some patent corpus bytes")) "deterministic"))
  (is (not= (cid/cidv1-raw "a") (cid/cidv1-raw "b")) "content-sensitive"))
