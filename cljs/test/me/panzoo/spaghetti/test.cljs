(ns me.panzoo.spaghetti.test
  (:require
    [me.panzoo.spaghetti :as s])
  (:use-macros
    [me.panzoo.cluj.macros.phantom :only (passert)]))

(def result (atom []))

(def fsm (s/state-machine
           :start
           {:start {\l :got_l}
            :got_l {\i :got_i}
            :got_i {\s :got_s}
            :got_s {\p :success}
            :success {}}
           :error-state :error
           :callback (fn [{:keys [transition new-state]}]
                       (when (not= :me.panzoo.spaghetti/reset transition)
                         (swap! result #(conj % new-state))))))

(passert (= :start (s/state fsm)))

(doseq [c "lisp"] (s/act fsm c))
(def r1 (first @result))
(passert (= @result [:got_l :got_i :got_s :success])
         (str "." (first @result) "." (last @result) ". FSM parsing failed."))
(reset! result [])
(s/reset fsm)

(doseq [c "lips"] (s/act fsm c))
(passert (= @result [:got_l :got_i :error :error])
         "FSM error state failed.")

(s/remove-state fsm :got_i)
(passert (not (contains? @(:graph fsm) :got_i))
         "Remove state failed.")

(def history (atom nil))
(def back (s/back-transition history))
(def rsm (s/state-machine
           :begin
           {:begin {:next :one}
            :one {:next :two}
            :two {:next :three}
            :three {}}
           :callback
           (s/history-callback
             history back
             (fn [{:keys [old-state transition new-state reverse?]}]
               (if reverse?
                 (reset! result [:back old-state transition new-state])
                 (reset! result [old-state transition new-state]))))))

(s/act rsm :next)
(s/act rsm :next)
(s/act rsm :next)
(passert (= :three (s/state rsm)))
(passert (= [:two :next :three] @result))

(s/act rsm back)
(passert (= :two (s/state rsm))
        (str "Back failed." (s/state rsm)))
(passert (= [:back :two :next :three] @result)
        (apply str "Back failed." @result))

(s/remove-state rsm :one)
(passert (= :two (s/state rsm)))
(passert (not (contains? @(:graph rsm) :one)))

(passert
  #(try (s/act rsm back) false
     (catch s/fsm-error _ true))
  "Back failed to throw fsm-error.")

(def history1 (atom nil))

(def back1 (s/back-transition history1))

(def qsm (s/state-machine
           :start
           {:start {:go :end}
            :end {}}
           :callback
           (s/history-callback history1 back1 (constantly nil))))

(passert
  #(try (s/act qsm back1) false
     (catch s/fsm-error e
       (= :empty-history (:type e))))
  "Back failed to throw empty-history error.")

(def dsm (s/state-machine
           :start
           {:start {:next :end}
            :end {}}
           :callback
           (s/transition-data-callback
             {:next (constantly 5)}
             {:end #(reset! result (* 2 (:transition-data %)))})))

(s/act dsm :next)
(passert (= 10 @result)
         "transition-data-callback failed.")

(.exit js/phantom 0)
