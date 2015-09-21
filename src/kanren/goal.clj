(ns kanren.goal
  (:require [kanren.state :as state]))

(defn pursue-in [goal state]
  (goal state))

(defn pursue-in-each [goal states]
  (when-let [[x & xs] states]
    (lazy-cat (pursue-in goal x)
              (pursue-in-each goal xs))))

(defmacro with-vars [vars goal-fn]
  `(fn [state#]
     (let [[state'# vars#] (->> (map keyword '~vars)
                                (state/create-variables state#))
           goal# (apply ~goal-fn vars#)]
       (pursue-in goal# state'#))))

(defn equal [a b]
  (fn [state]
    (when-let [state' (state/unify state a b)]
      (lazy-seq (list state')))))

(defn either [goal-a goal-b]
  (fn [state]
    (lazy-cat (pursue-in goal-a state)
              (pursue-in goal-b state))))

(defn both [goal-a goal-b]
  (fn [state]
    (let [states (pursue-in goal-a state)]
      (pursue-in-each goal-b states))))
