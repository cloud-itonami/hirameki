(ns hirameki.methods.cid
  "hirameki 閃き — kotoba IPFS content-address (CIDv1, raw 0x55, sha2-256, base32 'b').

  Delegates to the shared **com-junkawasaki/multiformats-clj** library — the SAME CID
  `ipfs add --cid-version=1 --raw-leaves` produces for a single raw block (< 256 KiB),
  verifiable with or without the `ipfs` daemon. Single-block by design (bounded R0
  snapshot). Public surface (cidv1-raw / single-block-limit) is unchanged."
  (:require [multiformats.core :as mf]))

(def single-block-limit (* 256 1024))

(defn cidv1-raw
  "CIDv1 / raw (0x55) / sha2-256 of the artifact. Input is a UTF-8 string."
  [^String s]
  #?(:clj (mf/cidv1-raw (.getBytes s "UTF-8"))
     :cljs (throw (ex-info "cidv1-raw is :clj-only" {}))))
