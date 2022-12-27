(ns day17
  (:require [clojure.string :as str]))

(repeat 1000 "hello") 

(defn inc-x [[x y]] [(inc x) y])
(defn dec-x [[x y]] [(dec x) y])
(defn dec-y [[x y]] [x (dec y)])
(defn add-h [h [x y]] [x (+ y h)])

(defn right [rock] (mapv inc-x rock)) 
(defn left [rock] (mapv dec-x rock)) 
(defn down [rock] (mapv dec-y rock))
(defn set-height [h rock] (mapv #(add-h (+ h 3) %) rock))
(defn max-y [rock] (apply max (mapv second rock)))

(defn open? [rock stack]
  (and (not-any? #(< % 0) (map second rock))
       (not-any? #(contains? stack %) rock)))

(defn next-cmd [{:keys [cmds max-idx] :as state}]
  (-> state
      (assoc :cmd (first cmds))
      (assoc :cmds (rest cmds))
      (update :ncmds inc)
      (update :idx #(if (< % max-idx) (inc %) 0))))

(defn next-rock [{:keys [rocks height] :as state}]
  (-> state
      (update :nrocks inc)
      (assoc :rock (set-height height (first rocks)))
      (assoc :rocks (rest rocks))))

(defn shift [{:keys [rock stack] :as state} f]
  (let [new-pos (f rock)]
    (if (and (every? #(< (first %) 7) new-pos)
             (every? #(>= (first %) 0) new-pos)
             (open? new-pos stack))
      (assoc state :rock new-pos)
      state)))

(defn merge-sets [acc [x y]] (update acc y #(conj % x)))
(defn update-cycles [{:keys [rock cycles height] :as state}]
  (let [c-height (count cycles)
        delta (- height c-height)
        cycles (->> (repeat delta #{})
                    (concat cycles)
                    (vec))]
    (assoc state :cycles (reduce merge-sets cycles rock))))

(defn shift-left [state] (shift state left))
(defn shift-right [state] (shift state right))
(defn move-down [{:keys [rock stack height] :as state}]
  (let [new-pos (down rock)]
    (if (open? new-pos stack)
      (assoc state :rock new-pos)
      (-> state
          (assoc :stack (apply conj stack rock))
          (assoc :height (max height (+ 1 (max-y rock))))
          (update-cycles)
          (next-rock)))))

(defn parse [c] (case c
                  \> shift-right
                  \< shift-left))

(def input (->> (slurp "data/day17.input")
                (str/trim)
                (map parse)))

(def s1 [[2 0] [3 0] [4 0] [5 0]])
(def s2 [[2 1] [3 0] [3 1] [3 2] [4 1]])
(def s3 [[2 0] [3 0] [4 0] [4 1] [4 2]])
(def s4 [[2 0] [2 1] [2 2] [2 3]])
(def s5 [[2 0] [3 0] [2 1] [3 1]])
          
(def command-queue (interleave (cycle input) (repeat move-down)))
(def shape-queue (cycle [s1 s2 s3 s4 s5]))
(def state {:rock nil :rocks shape-queue
            :cmd nil :cmds command-queue
            :idx 0 :max-idx (- (count input) 1) :ncmds 0
            :height 0 :nrocks 1
            :stack #{} :cycles []})


(def win 129)
(defn eq [lst t h] (= (subvec lst t (+ t win 1))
                      (subvec lst h (+ h win 1))))

(defn find-mu [lst lam n]
  (loop [mu 0, tor 0, har lam]
    (cond
      (or (= tor n) (= har n)) :not-found
      (= mu n) :not-found
      (eq lst tor har) {:start mu :length lam}
      :else (recur (inc mu) (inc tor) (min n (inc har))))))

(defn find-cycle [lst]
  (let [n (- (count lst) win)]
    (when (> n 100)
      (loop [lam 1, pow 1, tor 0, har 1]
        (cond
          (or (= tor n) (= har n)) :not-found
          (eq lst tor har) (find-mu lst lam n)
          (= pow lam) (recur 1 (* pow 2) (min n har) (min n (inc har)))
          :else (recur (inc lam) pow (min n tor) (min n (inc har))))))))

(defn debug [{:keys [rock idx nrocks ncmds height stack] :as state}]
  (let [chamber (vec (repeat (+ height 8) (vec (repeat 7 "-"))))]
    (->> (apply conj stack rock)
         (reduce #(assoc-in %1 (reverse %2) "#") chamber)
         (map str/join)
         (reverse))
         ; (run! println))
    (println (format "%5d %5d %5d %5d" idx ncmds height nrocks) (find-cycle (:cycles state)))))

(defn run [init]
  (loop [{:keys [cmd height nrocks cycles] :as state} (next-rock (next-cmd init))]
    ; (when (contains? (set (take 20 (iterate (partial + 2738) 104))) height) (debug state))
    (when (= (:idx state) 811) (debug state))
    (cond
      (= nrocks 10000)
      (do 
        (debug state)
        (println (subvec cycles 104 110))
        (println (subvec cycles 2842 2848))
        (println (subvec cycles 5580 5586))) ;(debug state))
      :else (-> (cmd state)
                (next-cmd)
                (recur)))))

(run state)
