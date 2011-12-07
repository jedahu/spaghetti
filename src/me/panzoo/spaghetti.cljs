; # Finite State Machine
;
; A simple state machine.
(ns me.panzoo.spaghetti
  (:require
    [goog.events :as events])
;<?
  (:use-macros
    [me.panzoo.jasmine :only (check it expect expect-not before)])
;?>
  )

(extend-type Atom
  IHash
  (-hash [o] (goog.getUid o)))

(defrecord fsm-error [type])

(defrecord StateMachine
  ;; Do not use this directly, use `state-machine` instead.
  [start callback graph])

(defn state-machine? [x]
  (instance? StateMachine x))

(defn reset
  "Reset the state machine. If present, `state` becomes the current state,
  otherwise the start state does. If call-callback? is true, the fsm callback
  is called with a transition argument of `::reset`."
  [fsm & {:keys [state call-callback?]}]
  (swap! (:current fsm)
         (fn [from-state]
           (let [to-state (or state (:start fsm))]
             (when call-callback?
               ((:callback fsm)
                  {:machine fsm
                   :from-state from-state
                   :transition ::reset
                   :to-state to-state}))
             to-state))))

(defn state-machine
  "Construct a new state machine. Requires a `start` state and a state `graph`.
  The graph is a map where each state is a key whose value is a map of
  transitions to states. E.g.:

      {:on {:down :off
            :up :on}
       :off {:down :off
             :up :on}}

  Available options are:

  :callback function [args]

  A function taking a single map, including at least these keys:
  `:machine`, `:from-state`, `:transition`, and `:to-state`.  Called
  after every transition.

  :error function

  The function to call when an invalid transition is attempted. If absent
  or `nil`, nothing will be done. It takes the same arguments as the callback.

  The start state is automatically entered with a transition of
  `::reset` from an old state of `start`"
  [start graph & {:as opts}]
  (let [fsm (StateMachine.
              start
              (or (:callback opts) (constantly nil))
              (atom graph)
              nil
              {:gensym (gensym)
               :watchlist (atom #{})
               :current (atom nil)
               :error (:error opts)
               :data (atom nil)})]
    (reset fsm :state start :call-callback? true)
    fsm))

(defn state
  "Get the current state."
  [fsm]
  @(:current fsm))

(def foo (atom 1))

;<?
(check "state-machine"
  (let [graph {:on {:down :off}}
        fsm (state-machine :on graph)]
    (it "should not modify the graph"
      (expect toEqual graph @(:graph fsm)))
    (it "should start in the start state"
      (expect toEqual :on (state fsm)))))
;?>

(defn act
  "Trigger the transition `trans` in the state machine `fsm`. Returns the new
  state. If the current state has no such transition `nil` will be returned,
  unless the state machine has an error state, in which case the error state
  will be entered and returned instead.

  `args` is a map to merge with the callback argument.
  function."
  [fsm trans & args]
  (swap! (:current fsm)
         (fn [from-state]
           (if-let [to-state (get-in @(:graph fsm) [from-state trans])]
             (do
               (swap! (:data fsm)
                      #((:callback fsm)
                          (merge {:machine fsm
                                  :from-state from-state
                                  :transition trans
                                  :to-state to-state
                                  :data %}
                                 (apply hash-map args))))
               to-state)
             (do
               (when-let [err (:error fsm)]
                 (err
                   (merge {:machine fsm
                           :from-state from-state
                           :transition trans
                           :to-state nil
                           :data @(:data fsm)})))
               from-state)))))

;<?
(check "act"
  (let [fsm (atom nil)
        err (atom nil)]
    (before
      (reset! fsm (state-machine
                    :on
                    {:on {:down :off}
                     :off {:up :on}}
                    :callback #(conj (or (:data %) [])
                                     [(:from-state %)
                                      (:transition %)
                                      (:to-state %)])
                    :error #(reset! err [(:from-state %)
                                         (:transition %)
                                         (:to-state %)]))))
    (it "should transition"
      (act @fsm :down)
      (expect toEqual :off (state @fsm)))
    (it "should give :callback the correct args"
      (act @fsm :down)
      (act @fsm :up)
      (expect toEqual [[:on :down :off] [:off :up :on]] @(:data @fsm)))
    (it "should call :error on unknown transitions"
      (act @fsm :unknown)
      (expect toEqual :on (state @fsm))
      (expect toEqual [:on :unknown nil] @err))))
;?>

(defn watch-ref [fsm trans r]
  (swap! (:watchlist fsm) #(conj % r))
  (add-watch r (:gensym fsm) #(act fsm trans :event %4)))

(defn unwatch-ref [fsm r]
  (swap! (:watchlist fsm) #(disj % r))
  (remove-watch r (:gensym fsm)))

(defn unwatch-all [fsm]
  (let [g (:gensym fsm)]
    (swap! (:watchlist fsm) #(doseq [r %] (remove-watch r g)))))

;<?
(check "watch unwatch"
  (let [fsm (state-machine
              :on
              {:on {:down :off}
               :off {:up :on}}
              :callback #(:event %))
        a (atom :x)]
    (it "should store watch"
      (watch-ref fsm :down a)
      (expect toBeTruthy (@(:watchlist fsm) a)))
    (it "should transition on watch change"
      (reset! a :y)
      (expect toEqual :off (state fsm))
      (expect toEqual :y @(:data fsm)))
    (it "should unwatch"
      (watch-ref fsm :up (atom :temp))
      (unwatch-ref fsm a)
      (expect toBeFalsy (@(:watchlist fsm) a))
      (expect toEqual 1 (count @(:watchlist fsm))))
    (it "should unwatch all"
      (unwatch-all fsm)
      (expect toEqual 0 (count @(:watchlist fsm))))
    (it "should not transition after unwatching"
      (reset! a :z)
      (expect toEqual :off (state fsm))
      (expect-not toEqual :z @(:data fsm)))))
;?>

#_(defn add-state
  "Add a new `state` to the state machine `fsm`. `transitions` is a map of
  transition keys to state values which are to be associated with `state`. If
  `state` already exists the new transitions will be merged with the existing
  ones."
  [fsm state & {:as transitions}]
  (swap! (:graph fsm)
         (fn [old]
           (update-in old [state] #(merge % transitions)))))

#_(defn remove-state
  "Remove a `state` from the state machine `fsm`. If `transition-keys` is
  supplied and even if it is empty, only those transitions are removed,
  otherwise the state and all associated transitions are removed."
  [fsm state & transition-keys]
  (swap! (:graph fsm)
         (fn [old]
           (if transition-keys
             (update-in old [state] #(apply dissoc % transition-keys))
             (dissoc old state)))))
