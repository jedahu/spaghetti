# Spaghetti

A simple state machine. Read the [docstrings](cljs/me/panzoo/spaghetti.cljs)
for more information.


## Synopsis

    (use 'me.panzoo.spaghetti)

    (def fsm (state-machine
               :closed
               {:closed {:open :opened}
                :opened {:close :closed}}
               :error-state :you-swipe-at-empty-air
               :callback (fn [old-state new-state]
                           (println "From " old-state " to " new-state))))

    (act fsm :open)
    ; From :closed to :opened

    (state fsm)
    ; => :opened

    (act fsm :close)
    ; From :opened to :closed

    (state fsm)
    ; => :closed

    (act fsm :close)
    ; From :closed to :you-swipe-at-empty-air

    (state fsm)
    ; => :you-swipe-at-empty-air

    (reset fsm)
    (state fsm)
    ; => :closed

Add and remove states/transitions:

    ;; Already existing state (:opened).
    (add-state fsm :opened {:open :broken-hinge})
    ; Merges new transition with existing :opened transitions.

    ;; New state.
    (add-state fsm :broken-hinge {:fix :closed})
    : Adds new state with transitions.

    (remove-state :opened {:open :broken-hinge})
    ; Removes transitions but not state.

    (remove-state :opened)
    ; Removes state and all associated transitions.
