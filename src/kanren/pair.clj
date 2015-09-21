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

(defn ->list [elements]
  (if-let [[x & xs] elements]
    (pair x (->list xs))))

(defn list-> [pair]
  (if (nil? pair)
    '()
    (let [x (left pair)
          xs (right pair)]
      (cons x (list-> xs)))))
