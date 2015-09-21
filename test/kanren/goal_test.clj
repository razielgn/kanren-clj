(ns kanren.goal-test
  (:require [clojure.test :refer :all]
            [kanren.state :refer :all]
            [kanren.pair :refer :all]
            [kanren.goal :as goal]))

(deftest test-goal-equal
  (let [[state [x y z]] (-> (make-state)
                            (create-variables [:x :y :z]))
        goal (goal/equal x 5)
        [goal-state] (doall (goal state))]
    (is (= {x 5} (values goal-state)))))

(deftest test-goal-with-variables
  (let [goal (goal/with-vars [x] #(goal/equal % 5))
        [state] (doall (goal (make-state)))
        [x] (variables state)]
    (is (= {x 5} (values state)))))

(deftest test-goal-with-either
  (let [goal (goal/with-vars [x] #(goal/either (goal/equal % 5)
                                               (goal/equal % 6)))
        [state-a state-b] (doall (goal (make-state)))
        [x-a] (variables state-a)
        [x-b] (variables state-b)]
    (is (= {x-a 5} (values state-a)))
    (is (= {x-b 6} (values state-b)))))

(deftest test-goal-with-both-simple
  (let [goal (goal/with-vars [x y] (fn [x y] (goal/both (goal/equal x 5)
                                                        (goal/equal y 7))))
        [state] (doall (goal (make-state)))
        [x y] (variables state)]
    (is (= {x 5 y 7} (values state)))))

(deftest test-goal-with-both-complex-1
  (let [goal (goal/with-vars [x y] (fn [x y] (goal/both (goal/equal x 7)
                                                        (goal/either (goal/equal y 5)
                                                                     (goal/equal y 6)))))
        [state-a state-b] (doall (goal (make-state)))
        [x-a y-a] (variables state-a)
        [x-b y-b] (variables state-b)]
    (is (= {x-a 7 y-a 5} (values state-a)))
    (is (= {x-b 7 y-b 6} (values state-b)))))

(deftest test-goal-with-both-complex-2
  (let [goal (goal/with-vars [x] (fn [x] (goal/both (goal/equal 1 x)
                                                    (goal/equal x 2))))
        states (doall (goal (make-state)))]
    (is (empty? states))))

(deftest goal-with-simple-pair
  (let [goal (goal/with-vars [x y] (fn [x y] (goal/equal (pair 3 x)
                                                         (pair y (pair 5 y)))))
        [state] (doall (goal (make-state)))
        [x y] (variables state)]
    (is (= {y 3 x (pair 5 3)}
           (values state)))))
