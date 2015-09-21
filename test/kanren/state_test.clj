(ns kanren.state-test
  (:require [clojure.test :refer :all]
            [kanren.state :refer :all]))

(deftest new-state-has-no-variables
  (let [state (make-state)]
    (is (= [] (variables state)))))

(deftest create-variables-returns-updated
  (let [[_ [x y z]] (-> (make-state)
                        (create-variables [:x :y :z]))]
    (is (= [(make-var 0 :x) (make-var 1 :y) (make-var 2 :z)]
           [x y z]))))

(deftest value-of-missing-binding
  (let [[state [x y z]] (-> (make-state)
                            (create-variables [:x :y :z]))]
    (is (= x (value-of state x)))))

(deftest value-of-present-binding
  (let [[state [x y z]] (-> (make-state)
                            (create-variables [:x :y :z]))
        state' (assign-values state {x y y z z 5})]
    (is (= 5 (value-of state' x)))))

(deftest unify-a-noop
  (let [[state [x y]] (-> (make-state)
                          (create-variables [:x :y]))
        state' (unify state x x)]
    (is (= {} (values state')))))

(deftest unify-two-variables
  (let [[state [x y]] (-> (make-state)
                          (create-variables [:x :y]))
        state' (unify state x y)]
    (is (= {x y} (values state')))
    (is (= y (value-of state' x)))))

(deftest unify-two-variables-plus-number
  (let [[state [x y]] (-> (make-state)
                          (create-variables [:x :y]))
        state' (-> state
                   (unify x y)
                   (unify x 5))]
    (is (= {x y y 5} (values state')))
    (is (= 5 (value-of state' x)))
    (is (= 5 (value-of state' y)))))

(deftest unify-two-variables-impossible
  (let [[state [x y]] (-> (make-state)
                          (create-variables [:x :y]))
        state' (-> state
                   (unify x y)
                   (unify x 5)
                   (unify y 6))]
    (is (= nil state'))))
