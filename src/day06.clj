(ns day06)

(def input (slurp "data/day06.input"))

(defn has-dup [[_ lst]] (< (count (set lst)) (count lst)))

(defn search [window]
  (->> (partition window 1 input)
       (map-indexed #(vector (+ window %1) %2))
       (drop-while has-dup)
       (first)))

(let [[n _] (search 4)] (println n))
(let [[n _] (search 14)] (println n))
