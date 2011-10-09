(ns me.panzoo.spaghetti.test
  (:require
    [me.panzoo.spaghetti :as s]))

(def result (atom []))

(def fsm (s/state-machine
           :start
           {:start {\l :got_l}
            :got_l {\i :got_i}
            :got_i {\s :got_s}
            :got_s {\p :success}
            :success {}}
           :error-state :error
           :callback (fn [old new] (swap! result #(conj % new)))))

(doseq [c "lisp"] (s/act fsm c))
(assert (= @result [:got_l :got_i :got_s :success])
        "FSM parsing failed.")
(reset! result [])
(s/reset fsm)

(doseq [c "lips"] (s/act fsm c))
(assert (= @result [:got_l :got_i :error :error])
        "FSM parsing failed.")
