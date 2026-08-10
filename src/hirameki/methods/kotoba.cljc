(ns hirameki.methods.kotoba
  "Compatibility adapter over the shared content-addressed Kotoba Datom library."
  (:require [kotoba.datom :as datom]))

(def add datom/add)
(def tx-cid datom/tx-cid)

(defn make-tx [datoms tx-id as-of prev-cid]
  (datom/make-tx datoms {:tx-id tx-id :as-of as-of :prev-cid prev-cid}))

(def append-tx datom/append-tx!)
(def read-log datom/read-log)
(def head-cid datom/head-cid)
(def verify-chain datom/verify-chain)
