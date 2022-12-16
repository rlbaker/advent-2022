(ns day01
  (:require [clojure.string :as s]))

(def input
  (map s/split-lines
       (s/split (slurp "data/day01.input") #"\n\n")))

(defn sum-cals [cals]
  (reduce + (map parse-long cals)))

(defn sum-top [n input]
  (->> input
       (map sum-cals)
       (sort-by -)
       (take n)
       (reduce +)))

(println (sum-top 1 input)) ; part 1
(println (sum-top 3 input)) ; part 2
