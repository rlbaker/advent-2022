(ns day14
  (:require [clojure.string :as string]))

(defn split-points [line] (string/split line #" -> "))
(defn split-coords [points] (map #(string/split % #",") points))
(defn to-int [[x y]] [(parse-long x) (parse-long y)])
(defn parse-points [points] (map to-int points))

(def input (->> (slurp "data/day14.input")
                (string/split-lines)
                (map split-points)
                (map split-coords)
                (map parse-points)))

(defn get-range [a b] (if (< a b)
                        (range a (+ b 1))
                        (range b (+ a 1))))

(defn line-points [[x1 y1] [x2 y2]]
  (for [y (get-range y1 y2)
        x (get-range x1 x2)]
    [x y]))

(defn points [wall] (mapcat #(apply line-points %) (partition 2 1 wall)))
(defn applyf [f pos m] (apply f (map pos (keys m))))

(def spout [500 0])
(def init (sorted-map spout "+"))
(def wall-points (mapcat points input))
(def grid (into init (map vector wall-points (repeat "#"))))
(def min-x (applyf min first grid))
(def min-y (applyf min second grid))
(def max-x (+ (applyf max first grid) 1))
(def max-y (+ (applyf max second grid) 1))

(defn project [min-x [x y]] [y (- x min-x)])
(defn kv [grid k] [k (grid k)])

(defn move [grid [x y]]
  (let [[c cv] (kv grid [x (+ y 1)])
        [l lv] (kv grid [(- x 1) (+ y 1)])
        [r rv] (kv grid [(+ x 1) (+ y 1)])]
    (cond
      (and (some? cv) (some? lv) (some? rv)) :drop
      (and (some? cv) (some? lv)) r
      (some? cv) l
      :else c)))
                           
(defn drop-sand [grid movef]
  (loop [pt spout] 
    (let [next-pt (movef grid pt)]
      (case next-pt
        :done [:done grid]
        :drop [:cont (assoc grid pt "o")] 
        (recur next-pt)))))

(defn run [part]
  (loop [grid grid
         n 0]
    (let [[action new-grid] (drop-sand grid part)]
      (case action
        :done n
        (recur new-grid (+ n 1))))))

(defn part1 [grid [x y]] 
  (if (or (< x min-x) (> x max-x) (< y min-y) (> y max-y))
    :done
    (move grid [x y])))
    
(defn part2 [grid [x y]] 
  (let [result (move grid [x y])]
    (cond
      (and (= result :drop) (= y 0)) :done
      (= y max-y) :drop
      :else result)))

(println (run part1))
(println (+ (run part2) 1))
