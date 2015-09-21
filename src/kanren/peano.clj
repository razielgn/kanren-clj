(ns kanren.peano
  (:require [kanren.pair :refer :all]))

(def ZERO :zero)
(def INC :+)

(defn ->peano [n]
  (if (zero? n)
    ZERO
    (pair INC (->peano (dec n)))))

(defn peano-> [peano]
  (if (= ZERO peano)
    0
    (inc (peano-> (right peano)))))
