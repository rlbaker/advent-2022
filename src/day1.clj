(ns day1
  (:require [clojure.string :as str]))

(def input
  (map str/split-lines
       (str/split (slurp "data/day1.input") #"\n\n")))

(defn sum-cals [cals]
  (reduce + (map parse-long cals)))

(defn sum-top [n input]
  (->> input
       (map sum-cals)
       (sort-by -)
       (take n)
       (reduce +)
       println))

(sum-top 1 input) ; part 1
(sum-top 3 input) ; part 2
