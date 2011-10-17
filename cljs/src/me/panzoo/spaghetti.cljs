;; # Finite State Machine
;;
;; A simple state machine.
(ns me.panzoo.spaghetti
  (:require
    [goog.events :as events]))

(defrecord fsm-error [type])

(declare reset)

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
  (let [fsm {:start start
             :callback (or (:callback opts) (constantly nil))
             :current (atom nil)
             :error-state (:error-state opts)
             :graph (atom graph)}]
    (reset fsm :state start :call-callback? true)
    fsm))

(defn act
  "Trigger the transition `trans` in the state machine `fsm`.  If the current
  state has no such transition an `fsm-error` will be thrown, unless the state
  machine has an error state, in which case the error state will be entered
  instead.

  `args` is a map to merge with the callback argument.
  function."
  [fsm trans & [{:as args}]]
  (swap! (:current fsm)
         (fn [old-state]
           (if-let [new-state (if (fn? trans)
                                (trans fsm args)
                                (or (get-in @(:graph fsm) [old-state trans])
                                    (:error-state fsm)))]
             (do ((:callback fsm)
                    (merge {:machine fsm
                            :old-state old-state
                            :transition trans
                            :new-state new-state}
                           args))
               new-state)
             (throw (fsm-error. :nonexistent))))))

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
  "Takes two maps, one with transition keys, the other with state keys. Both
  with function values. The functions take the same argument as the
  `state-machine` callback.

  Returns a callback for use with `state-machine`, which when called, calls the
  function associated with the transition and then the function associated with
  the new state. The return value from the transition function is made
  available to the new-state function under the `:transition-data` key. Clear
  as mud? The source is both shorter and easier to understand."
  [trans-callbacks state-callbacks]
  (fn [{:keys [new-state transition] :as args}]
    (let [state-cb (or (state-callbacks new-state) (constantly nil))
          trans-cb (or (trans-callbacks transition) (constantly nil))]
      (state-cb (assoc args :transition-data (trans-cb args))))))

(defn events-callback
  "Returns a callback for use with `state-machine` which effectively turns the
  result into an event driven state machine.

  Takes a map from transition keys to event values and a callback which takes
  the same argument as the `state-machine` callback.

  When a new state is entered the events associated with its transitions are
  listened to. When an event arrives the new state attached to that transition
  is entered and all listeners are cancelled.

  Each event value in `trans-evt-map` is a map with the following keys:

  :target The event target (a `goog.events.EventTarget` or a DOM node).

  :type The event type (e.g. `goog.events.EventType/CLICK`).

  :predicate (optional) A predicate function with an event arg, which is used
      to simulate a higher granularity event
      (e.g. `(fn [evt] (= 43 (. evt charCode)))`).

  :args (optional) A map of arguments to be passed to `act`."
  [trans-evt-map callback]
  (let [listener-keys (atom nil)]
    (fn [{:keys [machine new-state] :as args}]
      (callback args)
      (swap! listener-keys
             (fn [ks]
               (doseq [k ks] (events/unlistenByKey k))
               (loop [[[trans state] :as states] (@(:graph machine) new-state)
                      acc []]
                 (if (seq states)
                   (recur (rest states)
                          (if-let [evt (trans-evt-map trans)] 
                            (let [trans trans] ; immutable binding
                              (conj acc
                                    (events/listen
                                      (:target evt) (:type evt)
                                      (if-let [pred (:predicate evt)]
                                        (fn [e]
                                          (when (pred e)
                                            (act machine trans (:args evt))))
                                        (fn [_]
                                          (act machine trans (:args evt)))))))
                            acc))
                   acc))

               ;; FIX why is for not working?
               (comment for [[trans state] (@(:graph machine) new-state)
                       :when (trans-evt-map trans)]
                 (let [evt (trans-evt-map trans)]
                   (events/listen
                     (:target evt) (:type evt)
                     (if-let [pred (:predicate evt)]
                       (fn [_]
                         (when (pred)
                           (act machine trans (:args evt))))
                       (fn [_]
                         (act machine trans (:args evt))))))))))))

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
