(ns day1
  (:require advent clojure.pprint))

(def example (advent/groups "day1.example"))
(def input (advent/groups "day1.input"))

(defn to-ints [coll] (map parse-long coll))
(defn sum-cals [coll] (map #(reduce + (to-ints %)) coll))
(defn top-n [n coll] (take-last n (sort coll)))

(defn part1 [input] (apply + (top-n 1 (sum-cals input))))
(defn part2 [input] (apply + (top-n 3 (sum-cals input))))

(println (part1 input))
(println (part2 input))
