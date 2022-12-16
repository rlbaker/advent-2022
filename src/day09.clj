(ns day09
  (:require [clojure.string :as s]
            [clojure.math :as math]))

(defn to-cmd [[dir dist]] [(keyword dir) (parse-long dist)])

(def input
  (->> (slurp "data/day09.input")
       (s/split-lines)
       (map #(s/split % #" "))
       (map to-cmd)))

(defn dist [[x1 y1] [x2 y2]]
  (let [x (- x1 x2)
        y (- y1 y2)]
    (math/sqrt (+ (* x x) (* y y)))))

(defn move-head [[hx hy] dir]
  (case dir
    :R [(+ hx 1) hy]
    :U [hx (+ hy 1)]
    :L [(- hx 1) hy]
    :D [hx (- hy 1)]))

(defn clamp [t h]
  (let [delta (- h t)
        offset (max -1 (min 1 delta))]
    (+ t  offset)))

(defn move-tail [[hx hy :as h] [tx ty :as t]]
  (if (< (dist h t) 2) t
    [(clamp tx hx) (clamp ty hy)]))

(defn move-knots
  ([[h & knots] dir]
   (move-knots knots dir [(move-head h dir)]))

  ([[h & knots] dir acc]
   (if (nil? h) acc
     (let [new-t (move-tail (last acc) h)]
       (recur knots dir (conj acc new-t))))))

(defn move [[dir steps] [pos knots]]
  (if (= steps 0) [pos knots]
    (let [new-knots (move-knots knots dir)
          pos (conj pos (last new-knots))]
      (recur [dir (- steps 1)] [pos new-knots]))))

(defn run [[cmd & cmds] [pos knots]]
  (if (nil? cmd) (count pos)
    (recur cmds (move cmd [pos knots]))))

(def part1 (vec (repeat 2 [0 0])))
(println (run input [#{} part1]))

(def part2 (vec (repeat 10 [0 0])))
(println (run input [#{}  part2]))
