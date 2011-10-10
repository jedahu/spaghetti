(ns me.panzoo.spaghetti.reversible
  (:require
    [me.panzoo.spaghetti :as s]))

(defn- rsm-callback
  "Returns a FSM callback that pushes transition old-state pairs to a history
  stack. The supplied `callback` is called before any stack manipulation and
  takes the same arguments as the callback of
  `me.panzoo.spaghetti/state-machine`."
  [history callback]
  (fn [old trans new]
    (swap! history
           (fn [hist]
             ((or callback (constantly nil)) old trans new (peek hist))
             (conj hist [trans old])))))

(defn reversible-state-machine
  "Construct a new state machine. The arguments are the same as for
  `me.panzoo.spaghetti/state-machine` except for an additional
  `:reverse-callback` option.  

  The reverse callback takes the same arguments as the callback but is called
  by the `back` function.

  On each transition, the transition value is pushed on to a history stack, and
  on each call of the `back` function, the topmost value is popped off. Before
  this stack manipulation happens the appropriate callback is called."
  [start graph & {:as opts}]
  (let [hist (atom [])]
    (assoc
      (apply s/state-machine start graph opts)
      :history hist
      :reverse-callback (or (:reverse-callback opts) (constantly nil))
      :callback (rsm-callback hist (or (:callback opts) (constantly nil))))))

(defn back
  "Reverse the previous transition and return to the previous state. If the
  previous state no longer exists (see `me.panzoo.spaghetti/remove-state`) or
  the history stack is empty, the error state will be entered, if it exists,
  otherwise an `fsm-error` will be thrown. If an error state exists the first
  argument to the callback will be the error state. If an error is thrown the
  history stack is not touched."
  [fsm]
  (swap! (:history fsm)
         (fn [hist]
           (let [[trans state] (peek hist)
                 from-state (or (and (contains? @(:graph fsm) state)
                                     state)
                                (:error-state fsm))]
             (if from-state
               (swap! (:current fsm)
                      (fn [old-state]
                        ((:reverse-callback fsm)
                           from-state trans @(:current fsm))
                        from-state))
               (if (seq hist)
                 (throw (s/fsm-error. :nonexistent))
                 (throw (s/fsm-error. :empty-history)))))
           (pop hist))))
