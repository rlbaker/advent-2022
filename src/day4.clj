(ns day4
  (:require [clojure.string :as str]))

(def example (str/split-lines (slurp "data/day4.example")))
(def input (str/split-lines (slurp "data/day4.input")))

(defn part1 [s1 e1 s2 e2]
  (or (and (<= s1 s2) (>= e1 e2))
      (and (<= s2 s1) (>= e2 e1))))

(defn part2 [s1 e1 s2 e2]
  (and (>= e1 s2) (>= e2 s1)))

(defn run [part input]
  (->> input
     (map #(str/split % #"[,-]"))
     (map #(map parse-long %))
     (filter #(apply part %))
     (count)))

(println (run part1 input))
(println (run part2 input))
