(ns day3
  (:require [clojure.string :as s]
            [clojure.set :as cset]))

(def input (s/split-lines (slurp "data/day3.input")))

(defn priority [ch]
  (let [c (int (first ch))]
    (if (>= c 97)
      (- c 96)
      (- c 38))))

(defn find-item [bag]
  (let [n (/ (count bag) 2)]
   (cset/intersection
    (set (take n bag))
    (set (drop n bag)))))

(defn part1 [input]
  (->> input
       (map find-item)
       (map priority)
       (reduce +)))

(defn part2 [input]
  (->> input
       (map set)
       (partition 3)
       (map (partial apply cset/intersection))
       (map priority)
       (reduce +)))

(println (part1 input))
(println (part2 input))
