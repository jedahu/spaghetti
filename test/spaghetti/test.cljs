(ns spaghetti.test
  (:require
    [spaghetti :as s]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only (eq truthy)])
  (:use-macros
    [menodora :only (defsuite describe should expect)]))

(defsuite core-tests
  (describe "state-machine"
    :let [graph {:on {:down :off}}
          fsm (s/state-machine :on graph)]
    (should "not modify the graph"
      (expect eq graph (:graph fsm)))
    (should "start in the start state"
      (expect eq :on (:state fsm))))
  (describe "act"
    :let [err (atom nil)
          fsm (s/state-machine
                :on
                {:on {:down :off}
                 :off {:up :on}}
                :callback #(conj (or (:data %) [])
                                 [(:from-state %)
                                  (:transition %)
                                  (:to-state %)])
                :missing #(reset! err [(:from-state %)
                                       (:transition %)
                                       (:to-state %)]))]
    (should "transition"
      (expect eq :off (:state (s/act fsm :down))))
    (should "give :callback the correct args"
      (expect eq
        [[:on :down :off] [:off :up :on]]
        (-> fsm (s/act :down) (s/act :up) :data)))
    (should "call :missing on unknown transitions"
      (expect eq :on (:state (s/act fsm :unknown)))
      (expect eq [:on :unknown nil] @err)))
  (describe "watch unwatch"
    :let [fsm (s/state-machine
                :on
                {:on {:down :off}
                 :off {:up :on}}
                :callback #(:event %))
          fsmr (atom fsm)
          a (atom :x)]
    (should "store watch"
      (s/watch-ref fsmr :down a)
      (expect truthy ((:watchlist @fsmr) a)))
    (should "transition on watch change"
      (reset! a :y)
      (expect eq :off (:state @fsmr))
      (expect eq :y (:data @fsmr)))
    (should "unwatch"
      (s/watch-ref fsmr :up (atom :temp))
      (s/unwatch-ref fsmr a)
      (expect eq :off (:state @fsmr))
      (expect truthy (not ((:watchlist @fsmr) a)))
      (expect eq 1 (count (:watchlist @fsmr))))
    (should "unwatch all"
      (s/unwatch-all fsmr)
      (expect eq :off (:state @fsmr))
      (expect eq 0 (count (:watchlist @fsmr))))
    (should "not transition after unwatching"
      (reset! a :z)
      (expect eq :off (:state @fsmr))
      (expect truthy (not= :z (:data @fsmr)))))
  (describe "add remove state"
    :let [fsm (s/state-machine
                :one
                {:one {:incr :two}
                 :two {:incr :three
                       :decr :one}})]
    (should "add state"
      (expect eq
        {:incr :four}
        (get-in (s/add-state fsm :three {:incr :four})
                [:graph :three])))
    (should "merge transitions"
      (expect eq
        {:incr :two :decr :zero}
        (get-in (s/add-state fsm :one {:decr :zero})
                [:graph :one])))
    (should "remove state"
      (expect eq
        nil
        (get-in (s/remove-state fsm :two)
                [:graph :two])))
    (should "dissoc transitions"
      (expect eq
        {:decr :one}
        (get-in (s/remove-state fsm :two [:incr])
                [:graph :two])))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
