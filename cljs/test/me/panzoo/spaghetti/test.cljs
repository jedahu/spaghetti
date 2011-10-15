(ns me.panzoo.spaghetti.test
  (:require
    [me.panzoo.spaghetti :as s]
    [me.panzoo.spaghetti.reversible :as r]))

(def result (atom []))

(def fsm (s/state-machine
           :start
           {:start {\l :got_l}
            :got_l {\i :got_i}
            :got_i {\s :got_s}
            :got_s {\p :success}
            :success {}}
           :error-state :error
           :callback (fn [sm old trans new] (swap! result #(conj % new)))))

(doseq [c "lisp"] (s/act fsm c))
(def r1 (first @result))
(assert (= @result [:got_l :got_i :got_s :success])
        (str (first @result) "FSM parsing failed."))
(reset! result [])
(s/reset fsm)

(doseq [c "lips"] (s/act fsm c))
(assert (= @result [:got_l :got_i :error :error])
        "FSM error state failed.")

(s/remove-state fsm :got_i)
(assert (not (contains? @(:graph fsm) :got_i))
        "Remove state failed.")

(def rsm (r/reversible-state-machine
           :begin
           {:begin {:next :one}
            :one {:next :two}
            :two {:next :three}
            :three {}}
           :callback
           (fn [sm old trans new] (reset! result [old trans new]))
           :reverse-callback
           (fn [sm old trans new] (reset! result [:back old trans new]))))

(s/act rsm :next)
(s/act rsm :next)
(s/act rsm :next)
(assert (= :three (s/state rsm)))
(assert (= [:two :next :three] @result))

(r/back rsm)
(assert (= :two (s/state rsm))
        "Back failed.")
(assert (= [:back :two :next :three] @result)
        "Back failed.")

(s/remove-state rsm :one)
(assert (= :two (s/state rsm)))
(assert (not (contains? @(:graph rsm) :one)))

(assert
  (try (r/back rsm) false
    (catch s/fsm-error _ true))
  "Back failed to throw fsm-error.")


(def qsm (r/reversible-state-machine
           :start
           {:start {:go :end}
            :end {}}))

(assert
  (try (r/back qsm) false
    (catch s/fsm-error e
      (= :empty-history (:type e))))
  "Back failed to throw empty-history error.")
