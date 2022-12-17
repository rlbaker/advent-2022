(ns day16
  (:require [clojure.string :as s]
            [clojure.pprint]))


(def t (atom -1))

(defn parse-rooms [acc [room flow & tunnels]]
  (let [fl (parse-long flow)]
    (conj acc [(keyword room) {:flow fl
                               :open (if (zero? fl) true false)
                               :adj (set (map keyword tunnels))}])))

(def rooms (->> (slurp "data/day16.input")
                (s/split-lines)
                (map #(re-seq #"[A-Z]{2}|\d+" %))
                (reduce parse-rooms {})))


(def valves (into {} (remove #(zero? (:flow (second %))) rooms)))

(defn make-path [goal paths]
  (loop [path [goal]
         curr goal]
    (let [curr (get paths curr)]
      (cond
        (nil? curr) (reverse path)
        :else (recur (conj path curr) curr)))))

(defn find-next [curr [n & tail] visits paths scores]
  (if (nil? n)
    [visits paths scores]
    (let [tent-g (get scores curr 999)]
      (if (< tent-g (get scores n 999))
        (recur curr tail
               (conj visits n)
               (assoc paths n curr)
               (assoc scores n tent-g))
        (recur curr tail visits paths scores)))))

(defn path [start goal rooms]
  (loop [[visits paths scores] [#{start} {} {start 0}]]
    (let [curr (first visits)
          visits (disj visits curr)]
      (cond
        (nil? curr) :not-found
        (= curr goal) (pop (make-path goal paths))
        :else (recur (find-next curr (get-in rooms [curr :adj]) visits paths scores))))))

(defn total-new-flow [path rooms]
  (case path
    :not-found 0
    (/ (get-in rooms [(last path) :flow] 0) (+ 1 (* @t 2) (count path)))))
; (reduce + (map #(get-in rooms [% :flow]) path)))

(defn paths [start rooms]
  (->> (keys valves)
       (map #(path start % rooms))))

(defn weight [[s p]]
  (let [weight (/ (float s) (count p))]
    [weight p]))


(defn get-best [ps]
  (->> (map weight ps)
       (sort-by first)
       (last)
       (second)))

(defn open-valve [rooms room]
  (-> rooms
      (assoc-in [room :open] true)
      (assoc-in [room :flow] 0)))

(defn is-open [[_ v]] (v :open))

(def rate (atom 0))
(def total (atom 0))
(def shared-rooms (atom rooms))

(loop [curr :AA
       path [:AA]]
  (swap! t inc)
  (swap! total + @rate)
  (cond
    (>= @t 30) (println @total) 
    (nil? curr) (recur curr path)
    :else (let [ps (paths curr @shared-rooms)
                totals (map #(total-new-flow % @shared-rooms) ps)
                best (->> (map list totals ps)
                          (remove #(zero? (first %)))
                          (get-best))
                new-rate (get-in @shared-rooms [curr :flow])]
            (cond
              (= (last path) :AA)
              (recur (first best) (rest best))

              (nil? (first path)) 
              (do
                (swap! t inc)
                (swap! total + @rate)
                (swap! rate + new-rate)
                (swap! shared-rooms open-valve curr)
                (recur (first best) (rest best)))

              :else (recur (first path) (rest path))))))

