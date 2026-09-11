;; mesh.clj — hirameki 閃き KOTOBA Mesh entry component (Clojure / kotoba-clj).
;;
;; The mesh-hosting face of actor:hirameki (world public-patent KG mirror). Observatory
;; on-kse pattern (ADR-2606230001 §4): observes assignee→patent holding edges as Datom
;; assertions and derives exclusivity concentration via Datalog, routed to RELEASE.
;; The full per-field HHI / release-clock analysis stays in the actor's methods.
;;
;; Posture: G1 release MAP, NEVER a patent-busting / FTO / infringement / per-company
;; verdict; G2 a patent is the gated object, never a 取-holder; G6 no inventor person.
;; host-imports: kqe-assert! / kqe-query → kotoba:kais/kqe (needs cap/kqe)
(ns hirameki)

(defn observe []
  ;; observe — disclosed assignee → patent holdings (aggregate, no inventor person).
  (kqe-assert! "hirameki" "assignee-a" "holds" "patent-1")
  (kqe-assert! "hirameki" "assignee-a" "holds" "patent-2")
  (kqe-assert! "hirameki" "assignee-b" "holds" "patent-3")
  ;; derive — exclusivity concentration → release priority (Datalog).
  (kqe-query "release(?p) :- holds(?p)."))

(defn run [ctx] (observe))
(defn on-kse [topic payload] (observe))
