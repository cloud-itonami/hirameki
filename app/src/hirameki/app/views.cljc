(ns hirameki.app.views
  "hirameki console — the views, as pure hiccup over a summary map.

  Pure `.cljc`: every view is `data → hiccup`, so the page can be rendered by
  nbb at build time and the numbers in it can be asserted in a test without a
  browser.

  ## The design contract

  Components come from `jp-go-dds`; layout comes from `dds-ext-*`; spacing and
  type come from the `--hig-*` token contract that `tokens/bridge-css` puts on
  top of DADS. There is no app stylesheet beyond the few rules in `app-css`, and
  no raw hex, px font-size or hand-rolled grid anywhere in here.

  ## The editorial contract

  This corpus is 0.0003% of the world's patents and is shaped by the four seeds
  it was grown from. Every view that shows an aggregate also shows what the
  aggregate is OF — a share is meaningless without its base, and a top-holder
  table read without its provenance is how a citation-graph neighbourhood gets
  quoted as a fact about an industry."
  (:require [clojure.string :as str]
            [jp-go-dds.core :as dds]
            [hirameki.app.route :as route]))

;; ── small presentational helpers (token contract only) ───────────────────────

(def app-css
  "The whole app stylesheet. Everything else is the design system.

  `--hig-*` only — these all resolve through `tokens/bridge-css`, so this app
  follows DADS without knowing a single DADS primitive."
  (str/join
   "\n"
   [".stat-grid { display: grid; gap: var(--hig-spacing-4);"
    "  grid-template-columns: repeat(auto-fit, minmax(11rem, 1fr)); }"
    ".stat { border: 1px solid var(--hig-color-separator);"
    "  border-radius: var(--hig-radius-md); padding: var(--hig-spacing-4); }"
    ".stat dt { font-size: var(--hig-text-footnote-font-size);"
    "  color: var(--hig-color-secondary-label); margin: 0 0 var(--hig-spacing-1) 0; }"
    ".stat dd { font-size: var(--hig-text-title2-font-size);"
    "  font-weight: 600; margin: 0; font-variant-numeric: tabular-nums; }"
    ".stat dd small { font-size: var(--hig-text-footnote-font-size); font-weight: 400;"
    "  color: var(--hig-color-secondary-label); }"
    ".caveat { border-left: 3px solid var(--hig-color-separator);"
    "  padding-left: var(--hig-spacing-4); color: var(--hig-color-secondary-label);"
    "  font-size: var(--hig-text-footnote-font-size); }"
    ".bar { background: var(--hig-color-tertiary-system-fill); border-radius: var(--hig-radius-xs);"
    "  height: var(--hig-spacing-2); overflow: hidden; }"
    ".bar > span { display: block; height: 100%; background: var(--hig-color-secondary-label); }"
    "[data-view-panel][hidden] { display: none; }"]))

(defn- stat [label value & [note]]
  [:div {:class "stat"}
   [:dl
    [:dt label]
    [:dd (str value) (when note [:small (str " " note)])]]])

(defn- stats [& items] (into [:div {:class "stat-grid"}] items))

(defn- caveat [& children]
  (into [:p {:class "caveat"}] children))

(defn- pct [n total]
  (if (and total (pos? total)) (/ (Math/round (* 1000.0 (/ n (double total)))) 10.0) 0))

(defn- bar [n total]
  [:div {:class "bar" :role "presentation"}
   [:span {:style (str "width:" (pct n total) "%")}]])

;; ── views ────────────────────────────────────────────────────────────────────

(defn overview [{:keys [corpus-size harvested curated citations jurisdictions
                        assignees release as-of]}]
  [:div
   (dds/heading "現在地" {:level 2 :size "lg"})
   (stats
    (stat "特許" corpus-size (str "harvest " harvested " + curated " curated))
    (stat "引用エッジ" (get citations "edges"))
    (stat "管轄" (count jurisdictions))
    (stat "出願人（組織）" (get assignees "distinct_assignees")))
   (dds/divider)
   (dds/heading "この corpus が答えられないこと" {:level 3 :size "sm"})
   [:ul
    [:li [:strong "CPC 分類が無い。"]
     "Google Patents のページは CPC 記号を載せていないので、"
     "技術分野別の集中度はこの corpus からは計算できない。行は "
     [:code ":field \"UNKNOWN\""] " を持ち、タイトルからの推測はしない。"]
    [:li [:strong "世界の標本ではない。"]
     "4 件の種特許（CRISPR ×2・水性塗料・mRNA-LNP）から"
     "引用グラフを外向きに歩いた近傍で、約 2 億件に対して 0.0003%。"]]
   (caveat "as-of " as-of "。数値はすべて harvest 済みの実データから計算しており、"
           "外挿・推定は含まない。")])

(defn release-view [{:keys [release corpus-size ref-year]}]
  (let [order ["public-domain" "lapsed-released" "expiring-soon" "in-force" "pending"]
        rows (->> order
                  (keep (fn [k] (when-let [v (get release k)] [k v])))
                  vec)]
    [:div
     (dds/heading "解放クロック" {:level 2 :size "lg"})
     [:p "出願年 + 存続期間 − 基準年 で読む、開示済みの事実。"
      "有効性の判断でも侵害の判断でもない（G1/G3）。基準年 " (str ref-year) "。"]
     (dds/table
      {:caption (str "リリース状態の分布（n=" corpus-size "）")
       :headers ["状態" "件数" "割合" ""]
       :rows (mapv (fn [[k v]]
                     [k (str v) (str (pct v corpus-size) "%") (bar v corpus-size)])
                   rows)})
     (caveat "「pending」は Google Patents のページに登録日が無いもの — 実際に係属中とは限らず、"
             "ページが公開公報でその欄を持たない場合を含む。")]))

(defn holders [{:keys [assignees corpus-size]}]
  [:div
   (dds/heading "出願人集中" {:level 2 :size "lg"})
   (stats
    (stat "named-HHI" (get assignees "named_hhi") "0–1、下界")
    (stat "組織数" (get assignees "distinct_assignees"))
    (stat "組織に帰属" (get assignees "named"))
    (stat "帰属なし" (get assignees "unassigned") "分母から除外"))
   (dds/table
    {:caption (str "上位保有者（分母は組織に帰属する " (get assignees "named") " 件）")
     :headers ["出願人" "件数" "シェア" ""]
     :rows (mapv (fn [t] [(str/replace (get t "assignee") #"^:" "")
                          (str (get t "count"))
                          (str (get t "share") "%")
                          (bar (get t "count") (get assignees "named"))])
                 (get assignees "top"))})
   (caveat "HHI は下界。帰属の無い長い裾（" (get assignees "unassigned") " 件、"
           "個人出願のプレースホルダを含む）を分母から外しているので、"
           "実際の集中はこの値以上になる。"
           "そして上位が塗料に偏っているのは種特許の 1 件が水性塗料だったからで、"
           "塗料産業についての事実ではない。")])

(defn frontier [{:keys [citations corpus-size]}]
  (let [f (get citations "frontier")
        d (get citations "distinct_cited")]
    [:div
     (dds/heading "引用フロンティア" {:level 2 :size "lg"})
     (stats
      (stat "引用エッジ" (get citations "edges"))
      (stat "引用先（重複なし）" d)
      (stat "うち取得済み" (get citations "cited_already_harvested"))
      (stat "未取得＝フロンティア" f))
     [:p "1 tick = 1 lookup なので、フロンティアはそのまま "
      [:strong (str f " tick")] " 分の残作業を意味する。"
      "取得のたびにその特許の引用先が種に加わるため、フロンティアは縮むと同時に伸びる。"]
     (dds/table
      {:caption "引用グラフの閉じ具合"
       :headers ["" "件数" "割合"]
       :rows [["引用先のうち取得済み" (str (get citations "cited_already_harvested"))
               (str (pct (get citations "cited_already_harvested") d) "%")]
              ["引用先のうち未取得" (str f) (str (pct f d) "%")]]})
     (caveat "平均出次数 " (str (get citations "mean_out_degree"))
             "。corpus " (str corpus-size) " 件に対して未取得が " (str f) " 件あるということは、"
             "この近傍はまだ閉じていない。")]))

(defn provenance [{:keys [sources as-of]}]
  [:div
   (dds/heading "出所" {:level 2 :size "lg"})
   [:p "動いているソースと、動いていないソースを同じ表に並べる。"
      "実行されていないものを「対応済み」と書かないため。"]
   (dds/table
    {:caption "ソース別の状態"
     :headers ["ソース" "状態" "認証" "備考"]
     :rows (mapv (fn [{:keys [id status auth note]}]
                   [id
                    (dds/chip-label status
                                    {:type (if (= status "running") :solid-fill :outline)})
                    (or auth "—")
                    (or note "")])
                 sources)})
   (caveat "CPC 分類を供給できるのは USPTO ODP の経路だけで、これは実装済み・"
           "fixture でテスト済みだが、ライブ API に対して一度も実行されていない。"
           "as-of " as-of "。")])

(defn view-body [id data]
  (case id
    :overview (overview data)
    :release (release-view data)
    :holders (holders data)
    :frontier (frontier data)
    :provenance (provenance data)))

(defn document-body
  "The whole document: header, generated nav, and EVERY view rendered.

  All views are in the DOM and switched by `hidden`, so the page is complete for
  a reader without JavaScript, for a crawler, and for Ctrl-F — while still being
  one document with one mount."
  [data]
  (let [active (:id route/default-view)]
    [:div {:class "dds-ext-container"}
     [:header
      (dds/heading "hirameki 閃き" {:level 1 :size "xl"})
      [:p "世界の公開特許を、独占から commons への距離として観測する。"
       [:strong "解放の地図であって、侵害・FTO・特許価値の判定ではない"] "（G1）。"]
      (route/nav active)]
     (dds/divider)
     [:main
      (for [{:keys [id title summary]} route/views]
        [:section (cond-> {:data-view-panel (name id)
                           :id (route/view-dom-id id)
                           :aria-label title}
                    (not= id active) (assoc :hidden true))
         [:p {:class "caveat"} summary]
         (view-body id data)])]
     (dds/divider)
     [:footer
      [:p {:class "caveat"}
       "corpus: "
       [:a {:href "https://github.com/cloud-itonami/hirameki-patents"} "cloud-itonami/hirameki-patents"]
       "（shard ごとに CIDv1 で検証可能）· actor: "
       [:a {:href "https://github.com/cloud-itonami/hirameki"} "cloud-itonami/hirameki"]
       " · ADR-2606212200 / ADR-2607251552"]]]))
