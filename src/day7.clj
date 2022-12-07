(ns day7
  (:require [clojure.string :as str]))

(def input (slurp "data/day7.input"))

(defn split
  [lines]
  (map #(str/split % #" ") lines))

(defn parse-cmds
  [[input & output]]
  (let [[cmd path] input]
    (cond
      (= cmd "cd") [:cd path]
      (= cmd "ls") (conj output :ls))))

(defn parse-lines
  [input]
  (->> (str/split input #"\$ ")
       (drop 1)
       (map str/split-lines)
       (map split)))

(defn update-pos
  [pos [dir]]
  (if (= dir "..")
    (pop pos)
    (conj pos dir)))

(defn no-dirs [[info _]] (not= info "dir"))

(defn update-fs
  [fs pos data]
  (->> data
       (filter no-dirs)
       (map #(vector pos (parse-long (first %))))
       (reduce concat fs)))

(defn parse-fs
  [cmds]
  (loop [fs []
         pos []
         [[cmd & data] & cmds] cmds]
    (cond
      (= 0 (count cmds))
      (partition 2 fs)

      (= cmd :cd)
      (let [pos (update-pos pos data)]
        (recur fs pos cmds))

      (= cmd :ls)
      (let [fs (update-fs fs pos data)]
        (recur fs pos cmds)))))

(defn update-sums
  [sums dir size]
  (assoc sums dir (+ size (get sums dir 0))))

(defn sum-paths
  [sums [paths size]]
  (loop [sums sums
         parts paths]
    (if (<= (count parts) 0) sums
      (let [dir (str/join "/" parts)]
        (recur (update-sums sums dir size) (drop-last 1 parts))))))

(defn fs-sum [fs] (reduce sum-paths {} fs))

(def sums
  (->> input
       (parse-lines)
       (map parse-cmds)
       (parse-fs)
       (fs-sum)))

(defn sum-smallest
  [sum [_ size]]
  (if (< size 100000)
    (+ sum size)
    sum))

;part 1
(println (reduce sum-smallest 0 sums))

(def min-size (- 30000000 (- 70000000 (get sums "/"))))
(defn sizes [[_ size]] (>= size min-size))
(defn compare-size [[_ a] [_ b]] compare a b)

; part 2
(println (->> sums
             (filter sizes)
             (sort compare-size)
             (first)
             (second)))
