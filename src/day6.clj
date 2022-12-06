(ns day6)

(def input (slurp "data/day6.input"))

(defn zip [groups] (map vector (range) groups))

(defn match [group]
  (let [[n g] group]
    (< (count (set g)) (count g))))

(defn search [len]
  (let [groups (zip (partition len 1 input))
        [n _] (first (drop-while match groups))]
    (+ n len)))

(println (search 4)) ; part1
(println (search 14)) ; part2
