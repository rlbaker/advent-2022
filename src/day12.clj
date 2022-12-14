(ns day12
  (:require [clojure.string :as s]))

(def input
  (->> (slurp "data/day12.input")
       (s/split-lines)
       (mapv vec)))

(def height (count input))
(def width (count (first input)))

(def m (into {} (for [y (range height)
                      x (range width)]
                  [[y x] (get-in input [y x])])))

(def start (first (filter #(= (m %) \S) (keys m))))
(def end (first (filter #(= (m %) \E) (keys m))))
(def a-starts (filter #(= (m %) \a) (keys m)))

(defn adjacent [[y x]]
  [[(dec y) x]
   [y (dec x)]
   [(inc y) x]
   [y (inc x)]])

(defn oob? [[y x]]
  (or (< y 0) (>= y height) (< x 0) (>= x width)))

(defn to-int [c]
  (case c
    \S 97
    \E 122
    (int c)))

(defn invalid-step? [from to]
  (let [a (to-int (m from))
        b (to-int (m to))
        d (- b a)]
    (> d 1)))

(defn next-valid [pos visits]
  (remove #(or (visits %)
               (oob? %)
               (invalid-step? pos %))
          (adjacent pos)))

(defn with-true [coll] (zipmap coll (repeat true))) 
(defn with-dist [coll d] (map vector coll (repeat (+ d 1))))

(defn find-path [start]
  (loop [visits {start true}
         [[pos dist] & t :as q] [[start 0]]]
    (cond
      (empty? q) nil
      (= pos end) dist
      :else (let [adj (next-valid pos visits)
                  new-visits (merge visits (with-true adj))
                  new-q (vec (concat t (with-dist adj dist)))]
              (recur new-visits new-q)))))

; part 1
(println (find-path start))

; part 2
(println (first (remove nil? (sort (map find-path a-starts)))))
