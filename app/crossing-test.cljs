#!/usr/bin/env nbb
;; crossing-test.cljs — prove the nav ROUTES rather than NAVIGATES.
;;
;;   node scripts/resource-guard.mjs run build -- nbb app/crossing-test.cljs http://127.0.0.1:8731/
;;
;; This is the one property of a single-page app that is invisible in the source
;; (ADR-2608080100): a nav built from `<a href="#/x">` reads identically whether
;; the fragment router is wired or not, and a full document load looks exactly
;; like a correct view switch to the naked eye.
;;
;; So: stamp a value on `window`, cross every view, and check the stamp survived.
;; If the document reloaded, the stamp is gone.
;;
;; Two details learned the hard way and kept here on purpose:
;;   • wait for an element that exists ONLY in the target view. Waiting on
;;     something both views share (`main h1`) returns before the crossing has
;;     rendered, and the assertion passes for the wrong reason.
;;   • assert app state survives too, not just that no reload happened —
;;     without that, being one document bought nothing.
(ns crossing-test
  (:require ["playwright$default" :as pw]
            [clojure.string :as str]
            [promesa.core :as p]))

(def views ["overview" "release" "holders" "frontier" "provenance"])

(defn- fail! [msg]
  (println "  ✗" msg)
  (js/process.exit 1))

(defn -main []
  (let [base (or (nth (vec (js->clj js/process.argv)) 3 nil) "http://127.0.0.1:8731/")]
    (p/let [browser (.launch (.-chromium pw) #js {:headless true})
            page (.newPage browser)
            _ (.goto page base #js {:waitUntil "load"})

            ;; the stamp. A document load wipes it.
            _ (.evaluate page "window.__probe = {stamp: 'kept', crossings: 0}")
            boot (.evaluate page "window.__hirameki && window.__hirameki.booted")
            _ (when-not boot (fail! "router never booted — window.__hirameki is absent"))

            results
            (p/loop [remaining (rest views), acc []]
              (if (empty? remaining)
                acc
                (p/let [v (first remaining)
                        _ (.click page (str "nav [data-view=\"" v "\"]"))
                        ;; an element that exists ONLY in this view: its own panel,
                        ;; and specifically its panel becoming visible.
                        _ (.waitForSelector page (str "[data-view-panel=\"" v "\"]:not([hidden])")
                                            #js {:timeout 5000})
                        probe (.evaluate page "window.__probe && window.__probe.stamp")
                        booted (.evaluate page "window.__hirameki && window.__hirameki.booted")
                        crossings (.evaluate page "window.__hirameki && window.__hirameki.crossings")
                        visible (.evaluate page
                                           "Array.from(document.querySelectorAll('[data-view-panel]'))
                                            .filter(function(e){return !e.hidden})
                                            .map(function(e){return e.getAttribute('data-view-panel')})")
                        hash (.evaluate page "location.hash")]
                  (p/recur (rest remaining)
                           (conj acc {:view v :probe probe :booted booted
                                      :crossings crossings
                                      :visible (js->clj visible) :hash hash})))))
            _ (.close browser)]

      (println (str "crossing test — " (count results) " view changes from " base "\n"))
      (doseq [{:keys [view probe booted crossings visible hash]} results]
        (println (str "  " (if (= "kept" probe) "ok " "FAIL") " → " view
                      "  hash=" hash
                      "  visible=" (str/join "," visible)
                      "  crossings=" crossings
                      "  window.__probe=" (or probe "GONE"))))
      (let [lost (remove #(= "kept" (:probe %)) results)
            reboots (remove #(= boot (:booted %)) results)
            multi (remove #(= 1 (count (:visible %))) results)
            wrong (remove #(= (:view %) (first (:visible %))) results)]
        (cond
          (seq lost)
          (fail! (str "window.__probe was lost crossing to "
                      (str/join ", " (map :view lost))
                      " — the nav NAVIGATED (full document load) instead of routing"))

          (seq reboots)
          (fail! "window.__hirameki.booted changed — the document was re-executed")

          (seq multi)
          (fail! (str "more than one panel visible at "
                      (str/join ", " (map :view multi))))

          (seq wrong)
          (fail! (str "wrong panel visible at " (str/join ", " (map :view wrong))))

          (not= (count results) (:crossings (last results)))
          (fail! (str "hashchange fired " (:crossings (last results)) " times for "
                      (count results) " crossings"))

          :else
          (println (str "\n  one document held across " (count results)
                        " view changes; app state survived every one")))))))

(-main)
