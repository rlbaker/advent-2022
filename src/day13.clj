(ns day13
  (:require [clojure.string :as string]
            [clojure.edn :as edn]))

(defn split-groups [s] (string/split s #"\n\n"))
(defn parse-lists [coll] (map edn/read-string coll))

(def input (->> (slurp "data/day13.input")
                (split-groups)
                (map string/split-lines)
                (map parse-lists)))

(def n (count input))
(def idxs (range 1 (+ n 1)))

(defn check [a b]
  (cond
    (and (number? a) (number? b)) (compare a b)
    (and (number? a) (coll? b)) (check [a] b)
    (and (coll? a) (number? b)) (check a [b])
    (and (nil? a) (nil? b)) nil
    (and (coll? a) (coll? b)) (loop [[result & tail] (map check a b)]
                                (cond
                                  (= result -1) -1
                                  (= result 1) 1
                                  (nil? result) (check (count a) (count b))
                                  (zero? result) (recur tail)))))

; part 1
(println (->> input
             (map #(apply check %))
             (map list idxs)
             (filter #(= -1 (second %)))
             (map first)
             (reduce +)))

; part 2
(def sorted (as-> input coll
                 (apply concat coll)
                 (conj coll [[2]] [[6]])
                 (sort check coll)))

(println (* (.indexOf sorted [[2]])
            (.indexOf sorted [[6]])))
