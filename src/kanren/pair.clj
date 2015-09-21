(ns kanren.pair)

(defrecord Pair [left right])

(defn pair [left right]
  (Pair. left right))

(defn pair? [pair]
  (instance? Pair pair))

(defn right [pair]
  (:right pair))

(defn left [pair]
  (:left pair))
