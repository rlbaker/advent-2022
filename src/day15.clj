(ns day15)

(def input (->> (slurp "data/day15.input")
                (re-seq #"-?\d+")
                (map parse-long)
                (partition 4)))

(defn dist [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn parse-sensors [m [sx sy bx by]] (assoc m [sx sy] (dist [sx sy] [bx by])))
(defn parse-beacons [s [_ _ x y]] (conj s [x y]))

(def sensors (reduce parse-sensors {} input))
(def beacons (reduce parse-beacons #{} input))
(def max-d (apply max (map second sensors)))
(def min-x (- (reduce (fn [x [sx _ bx _]] (min x sx bx)) 0 input) max-d))
(def max-x (+ (reduce (fn [x [sx _ bx _]] (max x sx bx)) 0 input) max-d))

(defn check [pos]
  (reduce
    (fn [acc [sensor d]]
      (or acc (and (<= (dist pos sensor) d)
                   (not (contains? beacons pos)))))
    false
    sensors))

(defn find-covered []
  (loop [x min-x
         n 0]
   (cond
     (> x max-x) n
     (check [x 2000000]) (recur (inc x) (inc n))
     :else (recur (inc x) n))))

(defn get-range [y [[sx sy] d]]
  (let [r (max 0 (- d (abs (- y sy))))
        a (+ sx r)
        b (- sx r)]
    (if (zero? r) nil
      [(max 0 (min a b)) (min max-x (max a b))])))

(defn find-gaps [y]
  (let [ranges (sort (filter identity (map #(get-range y %) sensors)))]
    (loop [gaps []
           last-covered 0
           [[start end] & ranges] ranges]
      (cond
        (nil? start) nil
        (> start (inc last-covered)) (inc last-covered)
        :else (recur gaps (max last-covered end) ranges)))))

(defn find-signal []
  (loop [y 0]
    (let [gap (find-gaps y)]
      (cond
        (some? gap) [gap y]
        (= y 4000000) nil
        :else (recur (inc y))))))

; part 1
(println (find-covered))

; part 2
(let [[x y] (find-signal)]
  (println (+ (* x 4000000) y))) 
