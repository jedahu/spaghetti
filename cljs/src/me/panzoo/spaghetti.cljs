;; # Finite State Machine
;;
;; A simple state machine.
(ns me.panzoo.spaghetti)

(defrecord fsm-error [type])

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

  :callback function

  A function of three arguments, old-state, transition, and new-state. Called
  after every transition."
  [start graph & {:as opts}]
  {:start start
   :callback (or (:callback opts) (constantly nil))
   :current (atom start)
   :error-state (:error-state opts)
   :graph (atom graph)})

(defn act
  "Trigger the transition `trans` in the state machine `fsm`.  If the current
  state has no such transition an `fsm-error` will be thrown, unless the state
  machine has an error state, in which case the error state will be entered
  instead."
  [fsm trans]
  (swap! (:current fsm)
         (fn [old-state]
           (if-let [new-state (or (get-in @(:graph fsm) [old-state trans])
                                  (:error-state fsm))]
             (do ((:callback fsm) old-state trans new-state)
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
  is called with a transition argument of `::reset`."
  [fsm & {:keys [state call-callback?]}]
  (swap! (:current fsm)
         (fn [old-state]
           (let [new-state (or state (:start fsm))]
             (when call-callback?
               ((:callback fsm) old-state ::reset new-state))
             new-state))))
