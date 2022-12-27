(ns day19)

(defn parse-costs [[id a b c d e f]]
  {:id id
   :maxes [(max a b c e) d f 999]
   :ore [[a 0 0 0] [1 0 0 0]]
   :cla [[b 0 0 0] [0 1 0 0]]
   :obs [[c d 0 0] [0 0 1 0]]
   :geo [[e 0 f 0] [0 0 0 1]]})

(def blueprints
  (->> (slurp "data/day19.example")
       (re-seq #"\d+")
       (map parse-long)
       (partition 7)
       (map parse-costs)))

(def init [[0 0 0 0] [1 0 0 0]])

(defn buy [[mats robs queue] bp kind] 
  (let [maxes (:maxes bp)
        [cost new-rob] (kind bp)
        new-mats (mapv - mats cost)
        new-robs (mapv + robs new-rob)]
    (cond
      (some zero? (mapv - maxes new-robs)) [mats robs queue]
      (some neg? new-mats) [mats robs queue]
      :else [new-mats new-robs new-rob])))

(defn purchase [state bp]
  (-> #{state}
      (conj (buy state bp :geo))
      (conj (buy state bp :obs))
      (conj (buy state bp :cla))
      (conj (buy state bp :ore))))

(defn harvest [[mats robs]] [(mapv + mats robs) robs])

(defn debug [obj] (run! println obj) (println) obj)

(defn get-geodes [state]
  (-> state
      (first)
      (nth 3)))

(defn filter-max-geodes [states]
  (let [max-geodes (reduce #(max %1 (get-geodes %2)) -1 states)]
    (filter #(>= (get-geodes %) max-geodes) states)))

(defn next-states [state bp]
  (-> state
      (harvest)
      (purchase bp)
      (filter-max-geodes)))

(def bp (first blueprints))

(defn run [max-time states bp]
  (loop [t 0, states states]
    (let [states (filter-max-geodes states)]
      (if (< t max-time)
        (recur (inc t) (into #{} (mapcat #(next-states % bp) states)))
        states))))

(run 5 #{init} bp)
