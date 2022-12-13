(ns day12
  (:require [clojure.string :as str]))

(def input
  (->> (slurp "data/day12.input")
       (str/split-lines)
       (mapv vec)))

(def height (count input))
(def width (count (first input)))

(def m (into (sorted-map)
             (for [y (range height)
                   x (range width)]
               [[y x] (get-in input [y x])])))

(defn is-start [k] (= (get m k) \S))
(def start (first (filter is-start (keys m))))

(defn is-end [k] (= (get m k) \E))
(def end (first (filter is-end (keys m))))

(defn is-a [k] (= (get m k) \a))
(def a-starts (filter is-a (keys m)))

(defn adjacent [[y x]]
  [[(dec y) x] [y (dec x)] [(inc y) x] [y (inc x)]])

(defn oob? [[y x]]
  (or (< y 0) (>= y height) (< x 0) (>= x width)))

(defn to-int [c]
  (case c
    \S 96
    \E 123
    (int c)))

(defn invalid-step? [from to]
  (let [a (to-int (m from))
        b (to-int (m to))
        d (- b a)]
    (< 1 d)))

(defn next-valid [pos visits]
  (remove #(or (visits %)
               (oob? %)
               (invalid-step? pos %))
         (adjacent pos)))

(def visits-init (into (sorted-map) (map vector (keys m) (repeat false))))

(defn with-true [coll] (map vector coll (repeat true)))
(defn with-dist [coll d] (map vector coll (repeat (+ d 1))))

(defn find-path [start]
  (loop [visits (assoc visits-init start true)
         [h & t :as q] [[start 0]]
         min-steps 10]
    (let [[pos d] h]
      (cond
        (= pos end) d
        (empty? q) nil
        :else (let [adj (next-valid pos visits)
                    new-visits (into visits (with-true adj))
                    t (into [] (concat t (with-dist adj d)))]
                (recur new-visits t min-steps))))))

; part 1
(println (find-path start))

; part 2
(println (first (remove nil? (sort (map find-path a-starts)))))
