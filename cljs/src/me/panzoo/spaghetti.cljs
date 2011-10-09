;; # Finite State Machine
;;
;; A simple state machine.
(ns me.panzoo.spaghetti)

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

  A function of two arguments, old-state and new-state. Called after every
  transition."
  [start graph & {:as opts}]
  {:start start
   :callback (or (:callback opts) (constantly nil))
   :current (atom start)
   :error-state (:error-state opts)
   :graph (atom graph)})

(defn act
  "Trigger the transition `trans` in the state machine `fsm`.  If the current
  state has no such transition an error will be thrown, unless the state
  machine has an error state, in which case the error state will be entered
  instead."
  [fsm trans]
  (let [current @(:current fsm)
        target (get-in @(:graph fsm) [current trans])]
    (if-let [new-state (or target (:error-state fsm))]
      (swap! (:current fsm)
             (fn [old]
               ((:callback fsm) old new-state)
               new-state))
      (throw (js/Error. "Nonexistent target state.")))))

(defn state
  "Get the current state."
  [fsm]
  (:current fsm))

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
  supplied, only those transitions are removed, otherwise the state and all
  associated transitions are removed."
  [fsm state & transition-keys]
  (swap! (:graph fsm)
         (fn [old]
           (if transition-keys
             (update-in old [state] #(apply dissoc % transition-keys))
             (dissoc old state)))))

(defn reset
  "Reset the state machine. The start state becomes the current state."
  [fsm]
  (reset! (:current fsm) (:start fsm)))
