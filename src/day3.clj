(ns day3 (:require advent clojure.set))

(def example (advent/lines (slurp "data/day3.example")))
(def input (advent/lines (slurp "data/day3.input")))

(defn priority [ch]
  (let [c (int (first ch))]
    (if (>= c 97)
      (- c 96)
      (- c 38))))

(defn item [bag]
  (let [n (/ (count bag) 2)]
   (clojure.set/intersection
    (set (take n bag))
    (set (drop n bag)))))

(defn part1 [input]
  (->> input
       (map item)
       (map priority)
       (reduce +)))

(defn part2 [input]
  (->> input
       (map set)
       (partition 3)
       (map (partial apply clojure.set/intersection))
       (map priority)
       (reduce +)))

(println (part1 input))
(println (part2 input))

