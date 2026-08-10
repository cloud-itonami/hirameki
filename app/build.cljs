#!/usr/bin/env nbb
;; build.cljs — render the hirameki console to a single static document.
;;
;;   nbb --classpath "<see app/README.md>" app/build.cljs --dataset ../hirameki-patents
;;
;; One document, one mount (ADR-2608080100). Every view is rendered here, at
;; build time, from the real corpus — so the numbers on the page are the numbers
;; in the journal, not a hand-copied snapshot that drifts.
(ns build
  (:require ["node:fs" :as fs]
            ["node:path" :as path]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [jp-go-dds.page :as page]
            [jp-go-dds.tokens :as tokens]
            [toshokan-patents.quad :as quad]
            [hirameki.methods.corpus :as corpus]
            [hirameki.app.route :as route]
            [hirameki.app.views :as views]))

(defn- arg [argv flag dflt]
  (let [i (.indexOf (clj->js argv) flag)]
    (if (>= i 0) (nth argv (inc i)) dflt)))

(defn- slurp* [p] (fs/readFileSync p "utf8"))

(defn build-data
  "The summary the views render, straight from the corpus journal and the
  dataset's own provenance file. Nothing here is typed in by hand."
  [dataset-repo ref-year]
  (let [journal-dir (path/join dataset-repo "80-data" "public")
        rows (corpus/journal->patents (quad/read-sharded journal-dir "google-patents"))
        s (corpus/summarize rows ref-year)
        prov (edn/read-string (slurp* (path/join dataset-repo "ingest-provenance.edn")))]
    {:corpus-size (count rows)
     :harvested (count rows)
     :curated (get-in prov [:counts :curated] 0)
     :ref-year ref-year
     :as-of (str (first (str/split (:fetched_at prov "") #"/")) " 時点の harvest")
     :assignees (get s "assignees")
     :release (get s "release")
     :jurisdictions (get s "jurisdictions")
     :citations (get s "citations")
     :sources (mapv (fn [{:keys [id status auth note]}]
                      {:id id
                       :status (name status)
                       :auth (when (string? auth) auth)
                       :note note})
                    (:sources prov))}))

(defn render [data dds-css]
  (page/->page
   {:title "hirameki 閃き — 公開特許の解放クロック"
    :description (str "公開特許 " (:corpus-size data)
                      " 件の解放クロック・出願人集中・引用フロンティア。"
                      "解放の地図であって侵害/FTO の判定ではない。")
    :lang "ja"
    :css dds-css
    ;; bridge-css FIRST: it defines the --hig-* contract on DADS primitives, and
    ;; app-css consumes those variables. Reversed, every var resolves to nothing
    ;; and the layout silently collapses.
    :app-css (str tokens/bridge-css "\n" tokens/a11y-css "\n" views/app-css)
    :head [[:script {:type "application/json" :id "hirameki-summary"}
            (js/JSON.stringify (clj->js (dissoc data :sources)))]]}
   (views/document-body data)
   [:script route/router-script]))

(defn unbound-hig-tokens
  "Every `--hig-*` the page USES minus every `--hig-*` the page DEFINES.

  An unmapped token does not error, does not warn, and does not fall back: the
  declaration is simply dropped and the layout quietly loses a rule. This build
  shipped `--hig-color-label-secondary` and `--hig-color-fill-tertiary` before
  this check existed — both are real tokens under their HIG names
  (`--hig-color-secondary-label`, `--hig-color-tertiary-system-fill`), and the
  only symptom was text that was the wrong grey.

  A guess at a token name is indistinguishable from a correct one until someone
  looks at the page, so it has to be checked mechanically."
  [html]
  (let [used (set (map second (re-seq #"var\((--hig-[a-z0-9-]+)" html)))
        defined (set (map second (re-seq #"(--hig-[a-z0-9-]+)\s*:" html)))]
    (sort (remove defined used))))

(defn -main []
  (let [argv (vec (drop 3 (js->clj js/process.argv)))
        dataset (arg argv "--dataset" "../hirameki-patents")
        out (arg argv "--out" "app/dist")
        ref-year (js/parseInt (arg argv "--ref-year" "2026") 10)
        dds-css (slurp* (arg argv "--dds-css"
                             "../../orgs/kotoba-lang/jp-go-digital-design-system/resources/jp_go_dds/dds.css"))
        data (build-data dataset ref-year)
        html (render data dds-css)
        unbound (unbound-hig-tokens html)]
    (when (seq unbound)
      (println "BUILD FAILED — these --hig-* tokens resolve to nothing:")
      (doseq [t unbound] (println "  " t))
      (println "Fix the name, or add the token to jp-go-dds tokens/hig->dads upstream.")
      (println "Do NOT define it in app CSS — that is how the contract breaks.")
      (js/process.exit 1))
    (fs/mkdirSync out #js {:recursive true})
    (fs/writeFileSync (path/join out "index.html") html)
    ;; A 404 that sends only the addresses that really moved. NOT a catch-all
    ;; rewrite to "./": /x/y would land on /x/, also missing, and the fallback
    ;; would redirect to itself forever. Relative target so one artifact is
    ;; correct at any mount point.
    (fs/writeFileSync
     (path/join out "404.html")
     (str "<!DOCTYPE html>\n<meta charset=\"utf-8\">\n"
          "<title>hirameki — not found</title>\n"
          "<script>\n"
          "(function(){var m={'/release':'#/release','/holders':'#/holders',"
          "'/frontier':'#/frontier','/provenance':'#/provenance'};"
          "var h=m[location.pathname.replace(/\\/$/,'')];"
          "if(h){location.replace('./'+h);}})();\n"
          "</script>\n"
          "<p>そのアドレスはありません。<a href=\"./\">現在地へ</a></p>\n"))
    (println (str "wrote " (path/join out "index.html") "  "
                  (count html) " bytes  ·  " (:corpus-size data) " patents  ·  "
                  (count route/views) " views in one document"))))

(-main)
