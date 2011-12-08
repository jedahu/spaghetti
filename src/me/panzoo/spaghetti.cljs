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

(defn restart
  "Reset the state machine. If present, `state` becomes the current state,
  otherwise the start state does. If call-callback? is true, the fsm callback
  is called with a transition argument of `::restart`."
  [fsm & args]
  (let [{:keys [state call-callback?]} (apply hash-map args)
        to-state (or state (:start fsm))
        fsm* (assoc fsm :state to-state)]
    (if call-callback?
      (assoc
        fsm* :data
        ((:callback fsm)
           {:from-state (:state fsm)
            :transition ::restart
            :to-state to-state}))
      fsm*)))

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
  `::restart` from an old state of `start`"
  [start graph & {:as opts}]
  (let [fsm (StateMachine.
              start
              (or (:callback opts) (constantly nil))
              graph
              nil
              {:gensym (gensym)
               :watchlist #{}
               :state nil
               :error (:error opts)
               :data nil})]
    (restart fsm :state start :call-callback? false)))

(defn state
  "Get the current state."
  [fsm]
  (:state @fsm))

;<?
(check "state-machine"
  (let [graph {:on {:down :off}}
        fsm (state-machine :on graph)]
    (it "should not modify the graph"
      (expect toEqual graph (:graph fsm)))
    (it "should start in the start state"
      (expect toEqual :on (:state fsm)))))
;?>

(defn act
  "Trigger the transition `trans` in the state machine `fsm`. Returns the new
  state. If the current state has no such transition `nil` will be returned,
  unless the state machine has an error state, in which case the error state
  will be entered and returned instead.

  `args` is a map to merge with the callback argument.
  function."
  [fsm trans & args]
  (let [from-state (:state fsm)
        fsm* (assoc
               fsm :state
               (or (get-in (:graph fsm) [from-state trans])
                   from-state))]
    (if (= from-state (:state fsm*))
      (do
        (when-let [err (:error fsm)]
          (err
            (merge {:from-state from-state
                    :transition trans
                    :to-state nil
                    :data (:data fsm)}
                   (apply hash-map args))))
        fsm*)
      (assoc
        fsm* :data
        ((:callback fsm)
           (merge {:from-state from-state
                   :transition trans
                   :to-state (:state fsm*)
                   :data (:data fsm)}
                  (apply hash-map args)))))))

;<?
(check "act"
  (let [err (atom nil)
        fsm (state-machine
              :on
              {:on {:down :off}
               :off {:up :on}}
              :callback #(conj (or (:data %) [])
                               [(:from-state %)
                                (:transition %)
                                (:to-state %)])
              :error #(reset! err [(:from-state %)
                                   (:transition %)
                                   (:to-state %)]))]
    (it "should transition"
      (expect toEqual :off (:state (act fsm :down))))
    (it "should give :callback the correct args"
      (expect toEqual
              [[:on :down :off] [:off :up :on]]
              (-> fsm (act :down) (act :up) :data)))
    (it "should call :error on unknown transitions"
      (expect toEqual :on (:state (act fsm :unknown)))
      (expect toEqual [:on :unknown nil] @err))))
;?>

(defn restart!
  ""
  [fsmr & args]
  (swap! fsmr #(apply restart! % args)))

(defn act!
  ""
  [fsmr trans & args]
  (swap! fsmr #(apply act % trans args))
  nil)

(defn watch-ref [fsmr trans r]
  (swap! fsmr (fn [fsm]
                   (add-watch r (:gensym fsm)
                              #(act! fsmr trans :event %4))
                   (update-in fsm [:watchlist] #(conj % r))))
  nil)

(defn unwatch-ref [fsmr r]
  (swap! fsmr (fn [fsm]
                (remove-watch r (:gensym fsm))
                (update-in fsm [:watchlist] #(disj % r))))
  nil)

(defn unwatch-all [fsmr]
  (swap! fsmr (fn [fsm]
                (update-in fsm [:watchlist]
                           #(doseq [r %] (remove-watch r (:gensym fsm))))))
  nil)

;<?
(check "watch unwatch"
  (let [fsm (state-machine
              :on
              {:on {:down :off}
               :off {:up :on}}
              :callback #(:event %))
        fsmr (atom fsm)
        a (atom :x)]
    (it "should store watch"
      (watch-ref fsmr :down a)
      (expect toBeTruthy ((:watchlist @fsmr) a)))
    (it "should transition on watch change"
      (reset! a :y)
      (expect toEqual :off (:state @fsmr))
      (expect toEqual :y (:data @fsmr)))
    (it "should unwatch"
      (watch-ref fsmr :up (atom :temp))
      (unwatch-ref fsmr a)
      (expect toBeFalsy ((:watchlist @fsmr) a))
      (expect toEqual 1 (count (:watchlist @fsmr))))
    (it "should unwatch all"
      (unwatch-all fsmr)
      (expect toEqual 0 (count (:watchlist @fsmr))))
    (it "should not transition after unwatching"
      (reset! a :z)
      (expect toEqual :off (:state @fsmr))
      (expect-not toEqual :z (:data @fsmr)))))
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
