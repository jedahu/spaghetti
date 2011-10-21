;; # Finite State Machine
;;
;; A simple state machine.
(ns me.panzoo.spaghetti
  (:require
    [goog.events :as events]))

(defrecord fsm-error [type])

(declare reset)

(defrecord StateMachine
  ;; Do not use this directly, use `state-machine` instead.
  [start callback graph])

(defn state-machine? [x]
  (instance? StateMachine x))

(defn state-machine
  "Construct a new state machine. Requires a `start` state and a state `graph`.
  The graph is a map where each state is a key whose value is a map of
  transitions to states. E.g.:

      {on {:down off
           :up on}
       off {:down off
            :up on}}

  Available options are:

  :error-state value

  The state to switch to when an invalid transition is attempted. If absent
  or `nil` an error will be thrown instead.

  :callback function [args]

  A function taking a single map, including at least these keys:
  `:machine`, `:old-state`, `:transition`, and `:new-state`.  Called
  after every transition.

  The start state is automatically entered with a transition of
  `:me.panzoo.spaghetti/reset` from an old state of `start`"
  [start graph & {:as opts}]
  (let [fsm (StateMachine.
              start
              (or (:callback opts) (constantly nil))
              (atom graph)
              nil
              {:current (atom nil)
               :error-state (:error-state opts)})]
    (swap! (:graph fsm)
           (fn [g]
             (into {}
                   (for [[k v] g]
                     [(if (state-machine? k)
                        (assoc k :parent-machine fsm)
                        k)
                      v]))))
    (reset fsm :state start :call-callback? true)
    fsm))

(defn root-machine
  "The top-level parent of state machine `fsm`."
  [fsm]
  (if-let [p (:parent-machine fsm)]
    (recur p)
    fsm))

(defn act
  "Trigger the transition `trans` in the state machine `fsm`. Returns the new
  state. If the current state has no such transition `nil` will be returned,
  unless the state machine has an error state, in which case the error state
  will be entered and returned instead.

  `args` is a map to merge with the callback argument.
  function."
  [fsm trans & [args]]
  (swap! (:current fsm)
         (fn [old-state]
           (or (and (state-machine? old-state)
                    (apply act old-state trans args)
                    old-state)
               (when-let
                 [new-state (if (fn? trans)
                              (trans fsm args)
                              (or (get-in @(:graph fsm) [old-state trans])
                                  (:error-state fsm)))]
                 (do ((:callback fsm)
                        (merge {:machine fsm
                                :old-state old-state
                                :transition trans
                                :new-state new-state}
                               args))
                   new-state))))))

(defn state
  "Get the current state."
  [fsm]
  @(:current fsm))

(defn add-state
  "Add a new `state` to the state machine `fsm`. `transitions` is a map of
  transition keys to state values which are to be associated with `state`. If
  `state` already exists the new transitions will be merged with the existing
  ones."
  [fsm state & {:as transitions}]
  (swap! (:graph fsm)
         (fn [old]
           (update-in old [state] #(merge % transitions)))))

(defn remove-state
  "Remove a `state` from the state machine `fsm`. If `transition-keys` is
  supplied and even if it is empty, only those transitions are removed,
  otherwise the state and all associated transitions are removed."
  [fsm state & transition-keys]
  (swap! (:graph fsm)
         (fn [old]
           (if transition-keys
             (update-in old [state] #(apply dissoc % transition-keys))
             (dissoc old state)))))

(defn reset
  "Reset the state machine. If present, `state` becomes the current state,
  otherwise the start state does. If call-callback? is true, the fsm callback
  is called with a transition argument of `:me.panzoo.spaghetti/reset`."
  [fsm & {:keys [state call-callback?]}]
  (swap! (:current fsm)
         (fn [old-state]
           (let [new-state (or state (:start fsm))]
             (when call-callback?
               ((:callback fsm)
                  {:machine fsm
                   :old-state old-state
                   :transition :me.panzoo.spaghetti/reset
                   :new-state new-state}))
             new-state))))

(defn transition-data-callback
  "Takes a map with transition keys and function values, and a callback. The
  values in the map and the callback take the same argument as the
  `state-machine` callback.

  Returns a callback for use with `state-machine`, which when called, calls the
  function associated with the transition and then the supplied callback. The
  return value from the transition function is made available to the callback
  under the `:transition-data` key."
  [trans-callbacks callback] (fn [{:keys [new-state transition] :as args}]
    (let [trans-cb (or (get trans-callbacks transition) (constantly nil))]
      (callback (assoc args :transition-data (trans-cb args))))))

; Because `-hash` is not implemented for DOMWindow &co.
(defn- target-syms [evts]
  (loop [evts evts target-map {} done []]
    (if-let [target (:target (first evts))]
      (recur (rest evts)
             (if (some #(= % target) done)
               target-map
               (assoc target-map (gensym) target))
             (conj done target))
      target-map)))

; Because `-hash` is not implemented for DOMWindow &co.
(defn- get-sym [targets target]
  (loop [targets targets]
    (if-let [[sym targ] (first targets)]
      (if (= target targ)
        sym
        (recur (rest targets)))
      nil)))

(defn- trans-listener-keys
  [trans-evt-map {:keys [machine new-state]}]
  (let [evts (vec (for [[trans state] (@(:graph machine) new-state)
                        evt (get trans-evt-map trans)]
                    (assoc evt :transition trans)))
        targets (target-syms evts)]
    (vec
      (for [[[sym type] evts]
            (group-by (fn [x] [(get-sym targets (:target x)) (:type x)]) evts)]
        (events/listen
          (targets sym) type
          #(loop [[{:keys [predicate default?
                           propagate? transition args] :as e} & evts]
                  evts]
             (if (or (not predicate)
                     (predicate %))
               (do
                 (when-not default? (. % (preventDefault)))
                 (when-not propagate? (. % (stopPropagation)))
                 (act (root-machine machine) transition
                      (assoc args :event %)))
               (when (seq evts)
                 (recur evts)))))))))

(defn- state-listener-keys
  [state-listener-map {:keys [transition-data new-state]}]
  (let [evts (get state-listener-map new-state)
        targets (target-syms evts)]
  (vec
    (for [[[sym type] evts]
          (group-by
            (fn [x] [(get-sym targets (:target x)) (:type x)])
            evts)]
      (events/listen
        (targets sym) type
        #(loop [[{:keys [predicate default? propagate? callback]} & evts]
                evts]
           (if (or (not predicate)
                   (predicate %))
             (do
               (when-not default? (. % (preventDefault)))
               (when-not propagate? (. % (stopPropagation)))
               (callback % transition-data))
             (when (seq evts)
               (recur evts)))))))))

(defn events-callback
  "Returns a callback for use with `state-machine` which effectively turns the
  result into an event driven state machine.

  Takes a map from transition keys to sequences of events, a map from state
  keys to function values, and a callback. The callback takes the same argument
  as the `state-machine` callback. The functions take two arguments: a browser
  event and a transition-data map (for use with `transition-data-callback`).

  When a new state is entered the events associated with its transitions are
  listened to, and the function associated with the new state is called. The
  function should return a sequence of event values; these are also listened
  to. The listeners associated with the previous state and transition are
  cancelled. When an event associated with a transition is intercepted, that
  transition is triggered and this paragraph starts again.

  When a transition is triggered the browser event that triggered it is added
  to the `args` map under the key `:event`.

  An event value is a map with the following keys:

  :target The event target (a `goog.events.EventTarget` or a DOM node).

  :type The event type (e.g. `goog.events.EventType/CLICK`).

  :predicate (optional) A predicate function with an event arg, which is used
      to simulate a higher granularity event
      (e.g. `(fn [evt] (= 43 (. evt charCode)))`).

  :args (optional, only used for transitions) A map of arguments to be passed
      to `act`.

  :callback (only used for states) A function taking a single browser event.
  
  Events for the same browser event type and target should not overlap."
  [trans-evt-map state-listener-map callback]
  (let [listener-keys (atom nil)]
    (fn [{:keys [machine new-state] :as args}]
      (callback args)
      (swap!
        listener-keys
        (fn [ks]
          (doseq [k ks] (events/unlistenByKey k))
          (concat (trans-listener-keys trans-evt-map args)
                  (state-listener-keys state-listener-map args)))))))

(defn history-callback
  "Returns a `state-machine` callback that is called for both forwards and
  \"backwards\" transitions. This function must be used in concert with
  `back-transition`.

  `history` must be an atom containing a vector or nil.

  For forwards (normal) transitions, the created callback pushes `[transition
  old-state]` pairs onto the `history` stack.

  For backwards transitions, the latest pair is popped off the history stack
  and used in the args map to `callback`, which then contains at least the
  following keys:

  :reverse? true

  :old-state The current state.

  :new-state The state from the top of the history stack.

  :transition The transition from the top of the history stack. I.e. the one
      that was used to get from new-state to old-state.

  For a reset transition (`:me.panzoo.spaghetti/reset`) the history stack is
  set to nil.

  Note that `callback` is called before any history manipulation and takes the
  same argument as the callback for `state-machine`."
  [history back-fn callback]
  (fn [{:keys [transition old-state new-state] :as args}]
    (swap! history
           (fn [hist]
             (cond
               (= back-fn transition)
               (let [[trans _] (peek hist)]
                 (callback (assoc args
                                  :old-state new-state
                                  :new-state old-state
                                  :transition trans
                                  :reverse? true))
                 (pop hist))

               (= :me.panzoo.spaghetti/reset transition)
               (do (callback args)
                   nil)

               :else
               (do (callback args)
                   (conj hist [transition old-state])))))))

(defn back-transition
  "Creates a function which can be passed to `act` as a pseudo transition.
  Designed to be used with `history-callback` and must take the same atom as
  its `history` argument.

  Throws an `fsm-error` if the history stack is empty or if the state on the
  top of the stack no longer exists.

  (def history (atom []))
  (def back (back-transition history))
  (def callback (history-callback history (fn [args] ....)))
  (def fsm (state-machine :start <state-graph> :callback callback))

  ;; Forwards
  (act fsm :normal-transition)

  ;; Backwards
  (act fsm back)"
  [history]
  (fn [fsm & {:as args}]
    (let [hist @history]
      (if (seq hist)
        (let [[trans state] (peek hist)]
          (if (contains? @(:graph fsm) state)
            state
            (throw (fsm-error. :missing-state))))
        (throw (fsm-error. :empty-history))))))
