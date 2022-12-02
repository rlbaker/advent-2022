(ns day1 (:require advent))

(defn sum-cals [group] (reduce + (map parse-long group)))
(defn sum-groups [groups] (map sum-cals groups))
(defn top-n [n cals] (take-last n (sort coll)))
(defn sum-top [n cals] (apply + (top-n n coll)))

(defn part1 [input] (sum-top 1 (sum-groups input)))
(defn part2 [input] (sum-top 3 (sum-groups input)))

(def example (advent/groups "day1.example"))
(def input (advent/groups "day1.input"))

(println (part1 input))
(println (part2 input))
