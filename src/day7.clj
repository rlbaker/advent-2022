(ns day7-new
  (:require [clojure.string :as str]))

(def input (->> "data/day7.input"
                (slurp)
                (str/split-lines)
                (map #(str/split % #" "))))

(defn update-pos [pos dir]
  (case dir
    ".." (pop pos)
    (conj pos dir)))

(defn add-size [fs pos size]
  (if (empty? pos)
    fs
    (let [path (str/join "/" pos)
          fs (update fs path #(vec (conj % size)))]
      (recur fs (pop pos) size))))

(defn update-fs [fs pos [line & tail :as lines]]
  (if (nil? line) [fs pos nil]
    (let [[a _] line]
      (case a
        "$" [fs pos lines]
        "dir" (recur fs pos tail)
        (recur (add-size fs pos (parse-long a)) pos tail)))))

(defn process-line
  [fs pos [a b dir] tail]
  (case [a b]
    ["$" "cd"] [fs (update-pos pos dir) tail]
    ["$" "ls"] (update-fs fs pos tail)))

(defn process
  [[fs pos [line & tail]]]
  (if (nil? line) fs
    (recur (process-line fs pos line tail))))

(defn sum-vals [[path sizes]] [path (reduce + sizes)])

(def dir-sizes (->> [{} [] input]
                    (process)
                    (map sum-vals)
                    (into {})))

; ; part 1
(defn sum-smallest [sum [_ size]]
  (if (< size 100000)
    (+ sum size)
    sum))
(println (reduce sum-smallest 0 dir-sizes))

; ; part 2
(def min-size (- 30000000 (- 70000000 (get dir-sizes "/"))))
(defn large [[_ size]] (>= size min-size))
(defn compare-size [[_ a] [_ b]] compare a b)

(println (->> dir-sizes
              (filter large)
              (sort compare-size)
              (first)
              (second)))
