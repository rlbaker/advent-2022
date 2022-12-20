(ns day18
  (:require [clojure.set :as set]))

(def input (->> (slurp "data/day18.input")
               (re-seq #"\d+")
               (map parse-long)
               (partition 3)
               (map vec)
               (vec)))

(def lava (into #{} input))
(def max-area (* 6 (count lava)))
(def xs (map #(% 0) lava))
(def ys (map #(% 1) lava))
(def zs (map #(% 2) lava))

(defn neighbors [[x y z]]
  (sorted-set [(inc x) y z] [(dec x) y z]
              [x (inc y) z] [x (dec y) z]
              [x y (inc z)] [x y (dec z)]))

(defn adjacent [cube cubes]
  (set/intersection (neighbors cube) cubes))

; part 1
(->> lava
     (map #(adjacent % lava))
     (map count)
     (apply +)
     (- max-area)
     (println))
     
; part 2 ----
(defn min-of [ls] (- (apply min ls) 1))
(defn max-of [ls] (+ (apply max ls) 2))

(def chamber (into (sorted-set) (for [x (range (min-of xs) (max-of xs))
                                      y (range (min-of ys) (max-of ys))
                                      z (range (min-of zs) (max-of zs))]
                                 [x y z])))

(defn fill []
  (loop [found #{}, q (sorted-set [0 0 0])]
    (let [n (first q), q (disj q n)]
      (if (nil? n) found
        (recur (conj found n)
               (apply conj q (-> (neighbors n)
                                 (set/intersection chamber)
                                 (set/difference q lava found))))))))

; part 2 ----
(->> (fill)
     (map #(adjacent % lava))
     (map count)
     (apply +)
     (println))
