; # Spaghetti: a simple state machine
;
(defproject
  me.panzoo/spaghetti "0.0.6-SNAPSHOT"

  :description "Finite State Machine"

;<?
; Spaghetti's test code depends on [jasmine-clj], which is a collection of
; macros that wrap the [Jasmine] javascript test library.
;
; [jasmine-clj]: https://github.com/jedahu/jasmine-clj
; [Jasmine]: https://github.com/pivotal/jasmine
  :dependencies
  [[org.clojure/clojure "1.3.0"]
   [me.panzoo/jasmine "0.0.1-SNAPSHOT"]]
;?>
  )

; ## Synopsis
;
; <pre class='brush: clojure'>
; (use 'me.panzoo.spaghetti)
;
; (def fsm (atom
;            (state-machine
;              :closed
;              {:closed {:open :opened}
;               :opened {:close :closed}}
;              :missing (fn [{:keys [from-state transition]}]
;                         (.log js/console
;                               (str "There is no transition "
;                                    transition
;                                    " from "
;                                    from-state)))
;              :callback (fn [{:keys [old-state transition new-state}]
;                          (.log js/console
;                                (str "From "
;                                     from-state
;                                     " to "
;                                     to-state))))))
;
; (act! fsm :open)
; ; From :closed to :opened
;
; (:state @fsm)
; ; => :opened
;
; (act! fsm :close)
; ; From :opened to :closed
;
; (:state @fsm)
; ; => :closed
;
; (act! fsm :close)
; ; There is no transition :close from :closed
;
; (:state @fsm)
; ; => :closed
;
; (restart! fsm)
; (:state @fsm)
; ; => :closed
; </pre>
;
;
; ### Event driven transitions
;
; <pre class='brush: clojure'>
; (require '[goog.events :as events])
; (require '[goog.events.EventType :as event-type])
;
; (swap! fsm #(assoc % :callback (fn [{:keys [transition event]}]
;                                  (.log js/console
;                                        (str transition
;                                             " because of button "
;                                             (. event button))))))
;
; (def click (atom nil))
;
; (events/listen
;   js/window event-type/CLICK
;   (fn [evt] (reset! click evt)))
;
; (watch-ref fsm :open click)
;
; ; *left click*
; ; :opened because of button 0
;
; ; *right click*
; ; :opened because of button 2
; </pre>
;
; ### Add and remove states and transitions
;
; <pre class='brush: clojure'>
; ;; Already existing state (:opened).
; (add-state! fsm :opened {:open :broken-hinge})
; ; Merges new transition with existing :opened transitions.
;
; ;; New state.
; (add-state! fsm :broken-hinge {:fix :closed})
; : Adds new state with transitions.
;
; (remove-state! fsm :opened [:open])
; ; Removes the listed transitions (:open) but not the state (:opened).
;
; (remove-state! fsm :opened)
; ; Removes :opened state and all associated transitions.
; </pre>
;
;%include src/me/panzoo/spaghetti.cljs
;%include test.js
