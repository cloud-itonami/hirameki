(ns hirameki.app.route
  "The app's addressable views, and the fragment that addresses them.

  hirameki's console is a **single-page app** (kotoba-lang default, ADR-2608080100):
  one document, one mount; moving between the release clock and the citation
  frontier changes state, not location.

  ── why the fragment and not a path ─────────────────────────────────────────
  This is served from the cloud-itonami static sites plane. `pushState` to
  `/frontier` gives a URL that works until someone reloads it and the host
  answers 404. The fragment is never sent to the host, so a hash route survives
  reload, bookmarking and sharing with no server rewrite.

  ── why views are data ──────────────────────────────────────────────────────
  The nav is generated from `views`, so a view cannot exist without being
  reachable and cannot be reachable without appearing in the nav. A view added
  to the dispatch and forgotten in the nav is dead code that looks live.

  This file is the third copy of this shape (kami-app-daw, kami-app-nle, here),
  which is the extraction trigger the skill names. It is deliberately kept
  identical in structure so the extraction is a move, not a rewrite — and it
  does NOT belong in `jp-go-dds`, since routing is neither markup nor CSS."
  (:require [clojure.string :as str]
            [jp-go-dds.core :as dds]))

(def views
  "Every view, in nav order. The first is the default: an empty fragment on a
  fresh visit, and an unknown fragment, both resolve here rather than blanking
  the app."
  [{:id :overview  :fragment "#/"          :label "現在地"
    :title "現在地" :summary "corpus が今どれだけあり、何を答えられないか"}
   {:id :release   :fragment "#/release"   :label "解放クロック"
    :title "解放クロック" :summary "存続期間から見た commons への距離"}
   {:id :holders   :fragment "#/holders"   :label "出願人集中"
    :title "出願人集中" :summary "named-HHI と上位保有者（下界）"}
   {:id :frontier  :fragment "#/frontier"  :label "引用フロンティア"
    :title "引用フロンティア" :summary "まだ歩いていない引用先"}
   {:id :provenance :fragment "#/provenance" :label "出所"
    :title "出所" :summary "どのソースが動いていて、どれが動いていないか"}])

(def default-view (first views))

(defn fragment->view
  "Resolve a location fragment to a view. Unknown, empty and nil all land on the
  default — an address bar is user input, and a typo must not blank the app."
  [fragment]
  (let [f (or fragment "")]
    (or (first (filter #(= (:fragment %) f) views))
        ;; "#/holders?x=1" and "#holders" both mean the view.
        (first (filter #(and (not= "#/" (:fragment %))
                             (re-find (re-pattern (str "^#/?" (name (:id %)))) f))
                       views))
        default-view)))

(defn view-dom-id [id] (str "view-" (name id)))

(defn nav
  "The view switcher. `dds/button` with `:href` renders an anchor, so these are
  real links — middle-clickable, copyable, readable by anything that reads links
  — while looking like the design system's own controls.

  No app CSS: the row is `dds-ext-row`, the buttons are the design system's."
  [active-id]
  (into [:nav {:class "dds-ext-row" :aria-label "ビュー"}]
        (for [{:keys [id fragment label]} views
              :let [active? (= id active-id)]]
          (dds/button label {:type (if active? :solid-fill :text)
                             :size "sm"
                             :href fragment
                             :attrs (cond-> {:data-view (name id)}
                                      active? (assoc :aria-current "page"))}))))

(def router-script
  "Fragment routing, inline, ~30 lines.

  Every view is rendered into the document server-side and switched by toggling
  `hidden`, rather than mounted by a client bundle. For a read-only observatory
  that is the better trade: shipping React to re-render text that is already in
  the DOM costs megabytes and buys nothing, and it means the whole page is
  present for a reader with no JavaScript, for a crawler, and for `Ctrl-F`.

  It is still one document, one mount, fragment-addressed — the properties the
  single-page rule is actually about. `window.__hirameki` is what the crossing
  test asserts survives a view change: if it is gone, the nav navigated instead
  of routing."
  (str/join
   "\n"
   ["(function () {"
    "  window.__hirameki = window.__hirameki || { crossings: 0, booted: Date.now() };"
    "  var views = Array.prototype.slice.call(document.querySelectorAll('[data-view-panel]'));"
    "  var links = Array.prototype.slice.call(document.querySelectorAll('nav [data-view]'));"
    "  function idFor(hash) {"
    "    var h = hash || '';"
    "    for (var i = 0; i < views.length; i++) {"
    "      var v = views[i].getAttribute('data-view-panel');"
    "      if (h === '#/' + v || h === '#' + v || h.indexOf('#/' + v + '?') === 0) return v;"
    "    }"
    "    return views.length ? views[0].getAttribute('data-view-panel') : null;"
    "  }"
    "  function show(id) {"
    "    views.forEach(function (el) {"
    "      var on = el.getAttribute('data-view-panel') === id;"
    "      el.hidden = !on;"
    "    });"
    "    links.forEach(function (a) {"
    "      var on = a.getAttribute('data-view') === id;"
    "      if (on) { a.setAttribute('aria-current', 'page'); }"
    "      else { a.removeAttribute('aria-current'); }"
    "    });"
    "    window.__hirameki.view = id;"
    "  }"
    "  window.addEventListener('hashchange', function () {"
    "    window.__hirameki.crossings += 1;"
    "    show(idFor(location.hash));"
    "  });"
    "  show(idFor(location.hash));"
    "})();"]))
