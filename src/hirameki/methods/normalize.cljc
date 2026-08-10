(ns hirameki.methods.normalize
  "hirameki 閃き — pure normalizers shared by every row source.

  Two sources produce hirameki `:patent` rows — the USPTO ODP ingest
  (`ingest.cljc`) and the harvested Google Patents journal (`corpus.cljc`) — and
  a third (`dataset.cljc`) merges them. They must agree on what an assignee key
  is and on what a year is, or the same organization appears twice in one
  concentration reading under two keys.

  This namespace exists to break a dependency cycle *and* to make that agreement
  structural: there is one definition, not two that happen to match today."
  (:require [clojure.string :as str]))

(def ^:private legal-suffixes
  #"\b(inc|incorporated|corp|corporation|co|ltd|limited|llc|gmbh|kk|kabushiki kaisha|ag|sa|plc|nv|oyj|ab)\b")

(def ^:private placeholder-assignees
  "Strings that occupy the assignee slot without naming an organization.

  Google Patents writes `Individual` when a patent is held by natural persons
  rather than an org. Measured on the 618-patent harvest 2026-08-10, that would
  have entered the concentration table as the SECOND-largest holder at 6.0% —
  ahead of Nippon Paint — which is not a fact about anybody. It is the absence
  of an org, so it belongs in the `:unassigned` residual that concentration
  already excludes, exactly like `:other`.

  Mapping it to nil is also the G6-correct move: `Individual` is the one
  assignee value that stands for people."
  #{"individual" "individuals" "n/a" "na" "none" "unknown" "unassigned"})

(defn assignee->keyword
  "Normalize an ORGANIZATION name → a stable keyword, or nil if it is not one.

  Organizations only — never a person (G6). Legal-form suffixes are stripped so
  `Kansai Paint Co., Ltd.` and `Kansai Paint Co Ltd` are one holder rather than
  two, which is the difference between a correct concentration reading and a
  diluted one. Placeholders return nil so the caller can fold them into
  `:unassigned` rather than count them as a holder."
  [s]
  (when (and s (not (str/blank? s)))
    (let [cleaned (-> s str/lower-case str/trim
                      (str/replace #"[,\.]" "")
                      (str/replace legal-suffixes "")
                      str/trim
                      (str/replace #"\s+" "-"))]
      (when-not (or (str/blank? cleaned)
                    (contains? placeholder-assignees cleaned))
        (keyword cleaned)))))

(defn cpc->section
  "First character of a CPC symbol — the top-level technology section (A–H, Y)."
  [cpc]
  (when (and cpc (seq cpc)) (keyword (subs cpc 0 1))))

(defn cpc->subclass
  "First four characters of a CPC symbol — the subclass (e.g. `C12N`)."
  [cpc]
  (when (and cpc (>= (count cpc) 4)) (subs cpc 0 4)))

(defn year-of
  "Leading 4-digit year of an ISO-ish date string, or nil."
  [s]
  (when-let [m (and s (re-find #"(\d{4})" s))]
    (parse-long (second m))))

(defn merge-corpus
  "Fold `incoming` rows into `existing`, dedup by `:id`, sorted by `:id`.

  An `:authoritative` row UPGRADES a `:representative` row of the same id; the
  reverse never happens. So re-publishing after a harvest can only replace a
  hand-written stand-in with a mirrored fact, never overwrite a mirrored fact
  with a stand-in. Two authoritative rows keep the incoming one."
  [existing incoming]
  (let [auth? #(= :authoritative (:sourcing %))]
    (->> (reduce (fn [acc r]
                   (let [cur (get acc (:id r))]
                     (if (and cur (auth? cur) (not (auth? r)))
                       acc
                       (assoc acc (:id r) r))))
                 (into {} (map (juxt :id identity)) existing)
                 incoming)
         vals
         (sort-by :id)
         vec)))
