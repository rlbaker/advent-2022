(ns day6)

(def input (slurp "data/day6.input"))

(defn has-dup [[_ lst]] (< (count (set lst)) (count lst)))

(defn search [window]
  (->> (partition window 1 input)
       (map-indexed #(vector (+ %1 window) %2))
       (drop-while has-dup)
       (first)))

(let [[n _] (search 4)] (println n))
(let [[n _] (search 14)] (println n))
