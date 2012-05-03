;; # The code
;;
;; Spaghetti's core data structure is a record with immutable members. This
;; makes implementing and testing core functionality a simple stateless affair.
;; But this is not sufficient for a useful state machine given that it needs to
;; store state. One way to provide this storage yet retain immutability is to
;; use it with a state monad, but this wont do because the state machine may be
;; driven by events and clojurescript does not have access to the browser's
;; event loop. The solution this code uses is to store the data structure in an
;; atom and generate mutating equivalents for the pure functions that operate on
;; it.
(ns spaghetti

;; Only `cljs.core` is required by the production code.

m;<?
;; The tests use a few macros.
  (:use-macros
    [me.panzoo.jasmine :only (check it expect expect-not before)])
;;?>
  )


;; ## Helpers
;;
;; The code for driving the state machine by events stores atoms in a set, which
;; means they must implement the `-hash` method.
(extend-type Atom
  IHash
  (-hash [o] (goog.getUid o)))

;; `make!fn` takes a pure function and returns a function that atomically swaps
;; the contents of its first argument (which must be an atom) using that pure
;; function and the rest of its arguments.
(defn make!fn
  [f]
  (fn [a & args]
    (apply swap! a f args)
    nil))

(declare restart)


;; ## State machine
;;
;; A state machine object is an instance of this record, whose only purpose is
;; to enable the use of `instance?`. Don't use `StateMachine.` directly, use
;; `state-machine` instead.
(defrecord StateMachine [graph])


;; ### Graph
;;
;; A state machine requires a state graph and a start state. The state graph is
;; modeled as a map where each state is a key whose value is a map of
;; transitions to states. E.g.:
;;
;; <pre class='brush: clojure'>
;; {:on {:down :off
;;       :up :on}
;;  :off {:down :off
;;        :up :on}}
;; </pre>
;;
;;
;; ### Transition callback
;;
;; By itself this graph does nothing, so a function is added that is called
;; every time a transition is triggered. This callback function is passed a
;; single map containing at least these four keys:
;;
;; `:from-state` The state the machine is switching from.
;; `:transition` The transition precipitating the switch.
;; `:to-state` The state the machine is switching to.
;; `:data` What the previous invocation of this callback returned.
;;
;; More key-value pairs can be added by client code.
;;
;;
;; ### Missing transition callback
;;
;; The usual behaviour for a state machine when a non-existant transition is
;; triggered, is to just ignore it, make it a no-op. It can be convenient for
;; logging and testing purposes to keep track of these missing transitions. Thus
;; whenever a transition is attempted which does not exist, a function is called
;; with the same arguments as the transition callback, but its return value is
;; ignored.
;;
;;
;; ### Putting it all together
;;
;; The `state-machine` function creates a new [[StateMachine]] record using the
;; supplied `start` state and state `graph`. It takes a list of key-value
;; options including a transition callback under the `:callback` key, and a
;; missing transition callback under the `:missing` key.
(defn state-machine
  [start graph & opts]
  (let [opts (apply hash-map opts)]
    (-> (StateMachine.
          graph
          nil
          {:start start
           :callback (or (:callback opts) (constantly nil))
           :missing (or (:missing opts) (constantly nil))

;; The most recent `:callback` return value is stored under the `:data` key.
           :data nil

;; The current state is stored under the `:state` key.
           :state nil

;; This gensym value can be used to determine whether `not=` StateMachines are
;; persistent variants of each other. It is also used as a watch key by
;; [[watch-ref]].
           :gensym (gensym)})

;; The start state is automatically entered with a transition of
;; `::restart` from a `:from-state` of `start`
      (restart :state start))))

;;<?
(defn check-state-machine
  []
  (check "state-machine"
    (let [graph {:on {:down :off}}
          fsm (state-machine :on graph)]
      (it "should not modify the graph"
        (expect toEqual graph (:graph fsm)))
      (it "should start in the start state"
        (expect toEqual :on (:state fsm))))))
;;?>


;; ## Actions on a state machine
;;
;;
;; ### Restart
;;
;; A state machine can be reset to its start state or any other state. The
;; `restart` function handles this. It takes a state machine and two optional
;; key-value arguments and returns a modified machine.
(defn restart
  [fsm & args]
  (let [{:keys [state call-callback?]} (apply hash-map args)

;; If present, the `:state` option becomes the current
;; state, otherwise the start state set by [[state-machine]] does.
        to-state (or state (:start fsm))

;; If the `:call-callback?` option is true, the transition callback is called
;; with a `:transition` argument of `::restart`.
        maybe-call-callback (fn [f]
                              (if call-callback?
                                (assoc
                                  f :data
                                  ((:callback f)
                                     {:from-state (:state fsm)
                                      :transition ::restart
                                      :to-state to-state}))
                                f))]

;; The function does not check whether the restart state exists in the state
;; graph; it is assumed that the caller knows what it is doing.
    (-> (assoc fsm :state to-state)
        maybe-call-callback)))

;; `restart!` is a mutating variant of `restart`.
(def restart! (make!fn restart))

;;<?
;; The [[state-machine]] function is tested here because it requires [[restart]]
;; to be defined.
(check-state-machine)
;;?>


;; ### Act
;;
;; The state machine requires a mechanism for triggering transitions and calling
;; the callbacks described in the [[State machine]] section. `act` is a pure
;; function that triggers a transition on a state machine and returns the new
;; machine. If the transition exists, the machine's [[Transition callback]] will
;; be called and its return value stored under the machine's `:data` key,
;; otherwise the [[Missing transition callback]] will be called instead.
(defn act
  [fsm trans & args]

;; `args` is a list of key-value pairs to merge with the single map that is
;; passed to each callback.
  (let [args (apply hash-map args)
        from-state (:state fsm)
        to-state (get-in (:graph fsm) [from-state trans])]
    (if to-state
      (merge fsm
             {:state to-state
              :data ((:callback fsm)
                       (merge {:from-state from-state
                               :transition trans
                               :to-state to-state
                               :data (:data fsm)}
                              args))})
      (do
        ((:missing fsm)
           (merge {:from-state from-state
                   :transition trans
                   :to-state nil
                   :data (:data fsm)}
                  args))
        fsm))))

;; `act!` is a mutating variant of `act`.
(def act! (make!fn act))

;;<?
(check "act"
  (let [err (atom nil)
        fsm (state-machine
              :on
              {:on {:down :off}
               :off {:up :on}}
              :callback #(conj (or (:data %) [])
                               [(:from-state %)
                                (:transition %)
                                (:to-state %)])
              :missing #(reset! err [(:from-state %)
                                     (:transition %)
                                     (:to-state %)]))]
    (it "should transition"
      (expect toEqual :off (:state (act fsm :down))))
    (it "should give :callback the correct args"
      (expect toEqual
              [[:on :down :off] [:off :up :on]]
              (-> fsm (act :down) (act :up) :data)))
    (it "should call :missing on unknown transitions"
      (expect toEqual :on (:state (act fsm :unknown)))
      (expect toEqual [:on :unknown nil] @err))))
;;?>


;; ## Event driven transitions
;;
;; When events are mapped to transitions, the state machine becomes event
;; driven. Spaghetti achieves this in two steps:
;;
;; 1. atomic ref R is created and updated every time event E fires;
;; 2. a watch function is created from transition T of state machine atom M, and
;;    is added to ref R.
;;
;; The watch function is called whenever the content of ref R changes. It in
;; turn calls [[act!]] on M and T, with the new content of ref R appended under
;; the `:event` key.
;;
;; (Clojurescript has only atom refs. If this code is ever ported to Clojure,
;; hopefully little will need to change for STM refs and agents to work too.)
;;
;; The intermediary ref is not strictly necessary given that Clojurescript is
;; single threaded. That aside, having the state machine watch a ref can be
;; useful in other ways.
;;
;; Step one is left for client code to implement. Step two is performed by the
;; `watch-ref` function.
(defn watch-ref
  [fsmr trans r]
  (swap! fsmr (fn [fsm]

;; The `:gensym` value is used as a watch key because the state machine
;; structure is immutable (meaning modified objects will not have the same
;; identity), and because the atom the machine is stored in (`fsmr`) may not be
;; the only reference to it.
                   (add-watch r (:gensym fsm)
                              #(act! fsmr trans :event %4))

;; The reference `r` is added to the watchlist primarily to facilitate easy
;; unwatching of all references (by [[unwatch-all]]) when the state machine is
;; no longer needed. Note that the watchlist is actually a set.
                   (update-in fsm [:watchlist] #(conj (or % #{}) r))))
  nil)

;; References can be unwatched singly.
(defn unwatch-ref
  [fsmr r]
  (swap! fsmr (fn [fsm]
                (remove-watch r (:gensym fsm))
                (update-in fsm [:watchlist] #(disj % r))))
  nil)

;; Or they can be unwatched all at once.
(defn unwatch-all
  [fsmr]
  (swap! fsmr (fn [fsm]
                (update-in fsm [:watchlist]
                           #(doseq [r %] (remove-watch r (:gensym fsm))))))
  nil)

;;<?
(check "watch unwatch"
  (let [fsm (state-machine
              :on
              {:on {:down :off}
               :off {:up :on}}
              :callback #(:event %))
        fsmr (atom fsm)
        a (atom :x)]
    (it "should store watch"
      (watch-ref fsmr :down a)
      (expect toBeTruthy ((:watchlist @fsmr) a)))
    (it "should transition on watch change"
      (reset! a :y)
      (expect toEqual :off (:state @fsmr))
      (expect toEqual :y (:data @fsmr)))
    (it "should unwatch"
      (watch-ref fsmr :up (atom :temp))
      (unwatch-ref fsmr a)
      (expect toBeFalsy ((:watchlist @fsmr) a))
      (expect toEqual 1 (count (:watchlist @fsmr))))
    (it "should unwatch all"
      (unwatch-all fsmr)
      (expect toEqual 0 (count (:watchlist @fsmr))))
    (it "should not transition after unwatching"
      (reset! a :z)
      (expect toEqual :off (:state @fsmr))
      (expect-not toEqual :z (:data @fsmr)))))
;;?>


;; ## Modifying the graph
;;
;; It can be useful for states and transitions to be added on the fly. The two
;; pure functions [[add-state]] and [[remove-state]] take care of this, along
;; with their mutating counterparts [[add-state!]] and [[remove-state!]].
;;
;; `add-state` adds a new state to a state machine. It also takes a map of
;; transitions to destination states. These transitions will be merged with the
;; existing transitions for the new state (if any).
(defn add-state
  [fsm state transition-map]
  (update-in fsm [:graph state] #(merge % transition-map)))

;; `remove-state` removes a state from a state machine. It takes an optional
;; list of transitions. If that list is supplied, and even if it is empty, only
;; the transitions in the list are removed, otherwise the state and all its
;; transitions are removed.
(defn remove-state
  ([fsm state transitions]
   (update-in fsm [:graph state] #(apply dissoc % transitions)))
  ([fsm state]
   (update-in fsm [:graph] #(dissoc % state))))

;; `add-state!` is a mutating variant of `add-state`.
(def add-state! (make!fn add-state))

;; `remove-state!` is a mutating variant of `remove-state`.
(def remove-state! (make!fn remove-state))

;;<?
(check "add remove state"
  (let [fsm (state-machine
              :one
              {:one {:incr :two}
               :two {:incr :three
                     :decr :one}})]
    (it "should add state"
      (expect toEqual
              {:incr :four}
              (get-in (add-state fsm :three {:incr :four})
                      [:graph :three])))
    (it "should merge transitions"
      (expect toEqual
              {:incr :two :decr :zero}
              (get-in (add-state fsm :one {:decr :zero})
                      [:graph :one])))
    (it "should remove state"
      (expect toEqual
              nil
              (get-in (remove-state fsm :two)
                      [:graph :two])))
    (it "should dissoc transitions"
      (expect toEqual
              {:decr :one}
              (get-in (remove-state fsm :two [:incr])
                      [:graph :two])))))
;;?>
