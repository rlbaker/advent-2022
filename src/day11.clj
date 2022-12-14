(ns day11
  (:require [clojure.string :as s]))

(defn split-groups [s] (s/split s #"\n\n"))

(def input 
  (->> (slurp "data/day11.input")
       (split-groups)
       (map s/split-lines)))

(defn parse-num [s] (parse-long (re-find #"\d+" s)))
(defn parse-items [s] (mapv parse-long (re-seq #"\d+" s)))
(defn parse-op [s] (re-seq #"old|\d+|[\*\+\-\/]" s))

(defn arg [x worry]
  (if (= x "old")
    worry
    (parse-long x)))

(defn to-fn [op]
  (case op 
    "*" *
    "/" quot
    "+" +
    "-" -))

(defn apply-op
  [[a op b] worry]
  ((to-fn op) (arg a worry) (arg b worry)))

(declare lcm)

(defn parse-monkey
  [[id items ops tst t f]]
  (let [throw-t (parse-num t)
        throw-f (parse-num f)
        tst (parse-num tst)]
    {:id (parse-num id)
     :items (parse-items items)
     :op (partial apply-op (parse-op ops))
     :div tst
     :test #(if (zero? (rem % tst))
              [throw-t (mod % lcm)]
              [throw-f (mod % lcm)])}))

(def monkeys (mapv parse-monkey input))
(def lcm (apply * (map :div monkeys)))
(def inspects (atom (vec (repeat (count monkeys) 0))))

(defn throw-item [monkeys from-idx [to-idx item]]
  (let [from-items (get-in monkeys [from-idx :items])
        to-items (get-in monkeys [to-idx :items])
        monkeys (assoc-in monkeys [from-idx :items] (vec (rest from-items)))
        monkeys (assoc-in monkeys [to-idx :items] (conj to-items item))]
    monkeys))

(defn inspect-items [monkey]
  (let [op (:op monkey)
        items (:items monkey)]
    (map op items)))

(defn items [monkey]
  (map (:test monkey) (inspect-items monkey)))

(defn round [monkeys]
  (loop [n 0
         m monkeys]
    (if (>= n (count m)) m
      (let [items-to (items (nth m n))]
        (swap! inspects update n #(+ % (count items-to)))
        (recur (+ n 1) (reduce #(throw-item %1 n %2) m items-to))))))

(defn rounds [n monkeys]
  (if (zero? n) monkeys
    (recur (- n 1) (round monkeys))))

(rounds 10000 monkeys)
(println (apply * (take 2 (sort-by - @inspects))))
