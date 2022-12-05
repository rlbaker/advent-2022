(ns day2
  (:require [clojure.string :as str]))

(def input
  (map #(str/split % #" ") (str/split-lines (slurp "data/day2.input"))))

(def part1
  {"A" {"X" 4 "Y" 8 "Z" 3}
   "B" {"X" 1 "Y" 5 "Z" 9}
   "C" {"X" 7 "Y" 2 "Z" 6}})

(def part2
  {"A" {"X" 3 "Y" 4 "Z" 8}
   "B" {"X" 1 "Y" 5 "Z" 9}
   "C" {"X" 2 "Y" 6 "Z" 7}})

(defn sum-score [scores input]
  (reduce + (map #(get-in scores %) input)))

(println (sum-score part1 input))
(println (sum-score part2 input))
