(ns day10
  (:require [clojure.string :as s]))

(def input (->> "data/day10.input"
                (slurp)
                (s/split-lines)
                (map #(s/split % #" "))))

(defn lookup-cycles [[c _]] (contains? #{20 60 100 140 180 220} c))

(defn power [[c v]] (* c v))

(defn run [cpu [i v]]
  (let [[c x] (last cpu)]
    (case i
      "noop" (conj cpu [(+ c 1) x])
      "addx" (-> cpu
                 (conj [(+ c 1) x])
                 (conj [(+ c 2) (+ x (parse-long v))])))))

(def cpu-states (reduce run [[1 1]] input))

; part 1
(println (->> cpu-states
              (filter lookup-cycles)
              (map power)
              (reduce +)))

(defn idxs [i] [(quot (- i 1) 40) (rem (- i 1) 40)])
(defn visible? [x p] (or (= p (- x 1)) (= p x) (= p (+ x 1))))
(defn pixel [x p] (if (visible? x p) \# \.))

(def line (vec (repeat 40 0)))
(def crt (vec (repeat 6 line)))

(defn draw
  [crt [[c x] & states]]
  (if (empty? states) crt
    (let [[_ p :as pos] (idxs c)
          px (pixel x p)
          new-crt (assoc-in crt pos px)]
        (recur new-crt states))))

; part 2
(doseq [row (draw crt cpu-states)]
  (println (s/join row)))
