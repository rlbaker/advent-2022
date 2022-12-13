(ns day8
  (:require [clojure.string :as string]))

(defn digits [coll] (map #(Character/digit % 10) coll))

(def input
  (->> (slurp "data/day8.input")
       (string/split-lines)
       (map digits)
       (map vec)
       (vec)))

(def n (- (count input) 1))

(def indexes
  (for [y (range 1 n)
        x (range 1 n)]
    [y x]))

(defn up [[y x]] [(- y 1) x])
(defn down [[y x]] [(+ y 1) x])
(defn left [[y x]] [y (- x 1)])
(defn right [[y x]] [y (+ x 1)])

(defn edge [[y x]] (or (or (= y 0) (= y n))
                       (or (= x 0) (= x n))))

(defn visible
  [idx h _ movef]
  (let [idx (movef idx)]
    (cond
      (>= (get-in input idx) h) false
      (edge idx) true
      :else (recur idx h 0 movef))))

(defn score
  [idx h acc movef]
  (let [idx (movef idx)]
    (cond
      (>= (get-in input idx) h) acc
      (edge idx) acc
      :else (recur idx h (+ acc 1) movef))))

(defn march [marchf idx]
  (let [h (get-in input idx)]
    (map #(marchf idx h 1 %) [up down left right])))

(def part1
  (->> (for [idx indexes] (march visible idx))
       (map #(some identity %))
       (filter identity)
       (count)
       (+ (* n 4))))

(def part2
  (->> (for [idx indexes] (march score idx))
       (map #(reduce * %))
       (sort)
       (last)))

(println part1)
(println part2)
