(ns kanren.state
  (:require [kanren.pair :refer :all]))

(defrecord State [counter variables values])
(defrecord Variable [id label])

(defn make-var [id label]
  (Variable. id label))

(defn make-state []
  (State. 0 [] {}))

(defn variables [state]
  (:variables state))

(defn values [state]
  (:values state))

(defn- create-variable [state label]
  (let [variable (Variable. (:counter state) label)
        state' (-> state
                   (update-in [:counter] inc)
                   (update-in [:variables] #(conj % variable)))]
    [state' variable]))

(defn create-variables [state labels]
  (reduce (fn [[state variables] label]
            (let [[state' variable] (create-variable state label)]
              [state' (conj variables variable)]))
          [state []]
          labels))

(defn assign-values [state values]
  (update-in state [:values] #(conj % values)))

(defn value-of [state k]
  (let [values (:values state)]
    (loop [k k]
      (cond
        (contains? values k)
          (recur (get values k))
        (pair? k)
          (pair (value-of state (left k))
                (value-of state (right k)))
        :else k))))

(defn unify [state a b]
  (let [a (value-of state a)
        b (value-of state b)]
    (cond
      (= a b)
        state
      (instance? Variable a)
        (assign-values state {a b})
      (instance? Variable b)
        (assign-values state {b a})
      (and (pair? a) (pair? b))
        (if-let [state' (unify state (left a) (left b))]
          (unify state' (right a) (right b))))))
