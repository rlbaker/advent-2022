(ns day1
  (:require advent))

(defn to-ints [coll] (map parse-long coll))
(defn sum-ints [coll] (map #(reduce + (to-ints %)) coll))
(defn top-n [n coll] (take-last n (sort coll)))

(defn part1 [input] (apply + (top-n 1 (sum-ints input))))
(defn part2 [input] (apply + (top-n 3 (sum-ints input))))

; (def input (advent/groups "day1.example"))
(def input (advent/groups "day1.input"))

(println (part1 input))
(println (part2 input))
