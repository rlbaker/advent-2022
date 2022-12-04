(ns day1
  (:require [advent]))

(def example (advent/split-groups (slurp "data/day1.example")))
(def input (advent/split-groups (slurp "data/day1.input")))

(defn sum-group [group] (reduce + (map parse-long group)))

(defn sum-top [n input]
  (->> input
       (map sum-group)
       (sort)
       (take-last n)
       (apply +)))

(defn part1 [input] (sum-top 1 input))
(defn part2 [input] (sum-top 3 input))

(println (part1 input))
(println (part2 input))
