(ns day20)

(defrecord Node [steps prev next])

(defn build-node [n mult]
  (fn [[i steps]]
    (let [prev (if (zero? i) n (dec i))
          next (if (< i n) (inc i) 0)]
      (Node. (* steps mult) prev next))))

(def input (->> (slurp "data/day20.input")
                (re-seq #"-?\d+")
                (map parse-long)))

(def zero-idx (.indexOf input 0))
(def len (count input))
(def N (dec len))

(defn remove-node [data node]
  (let [prev (get-in data [node :prev])
        next (get-in data [node :next])]
    (-> data
        (assoc-in [prev :next] next)
        (assoc-in [next :prev] prev))))

(defn insert-node [data node target]
  (let [next (get-in data [target :next])]
    (-> data
        (assoc-in [target :next] node)
        (assoc-in [node :prev] target)
        (assoc-in [node :next] next)
        (assoc-in [next :prev] node))))

(defn walk [data node steps]
  (loop [curr node
         n (mod (dec steps) N)]
    (if (pos? n)
      (recur (get-in data [curr :next]) (dec n))
      curr)))

(defn mix [data]
  (loop [data data, i 0]
    (if (< i len)
      (let [{:keys [steps next]} (get data i)
            data (remove-node data i)
            pos (walk data next steps)]
        (-> data
            (insert-node i pos)
            (recur (inc i))))
      data)))

(defn mix-n [data n] (nth (iterate mix data) n))

(defn lookup [data]
  (loop [curr zero-idx, n 0, values []]
    (let [{:keys [steps next]} (get data curr)]
      (cond
        (= n 3000) (conj values steps)
        (or (= n 1000) (= n 2000)) (recur next (inc n) (conj values steps))
        :else (recur next (inc n) values)))))

(def part1-data (->> input
                     (map vector (range))
                     (map (build-node N 1))
                     (into [])))

(def part2-data (->> input
                     (map vector (range))
                     (map (build-node N 811589153))
                     (into [])))

(time (->> (mix part1-data)
           (lookup)
           (apply +)
           (println)))

(time (->> (mix-n part2-data 10)
           (lookup)
           (apply +)
           (println)))
