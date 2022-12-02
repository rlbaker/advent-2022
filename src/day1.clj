(ns day1 (:require advent))

(def example (advent/groups (slurp "data/day1.example")))
(def input (advent/groups (slurp "data/day1.input")))

(defn sum-cals [group] (reduce + (map parse-long group)))
(defn sum-groups [groups] (map sum-cals groups))
(defn top-n [n cals] (take-last n (sort cals)))
(defn sum-top [n cals] (apply + (top-n n cals)))

(defn part1 [input] (sum-top 1 (sum-groups input)))
(defn part2 [input] (sum-top 3 (sum-groups input)))

(println (part1 input))
(println (part2 input))
