(ns day5
  (:require [clojure.string :as str]))

(defn read-blocks [line] (mapv (vec line) (range 1 (count line) 4)))
(defn clean [col] (drop-while #(= \space %) col))
(defn parse-stacks [lines]
  (vec (->> (map read-blocks lines)
            (apply map list)   ; transpose the rows into stacks
            (map clean))))

(defn split-cmd [line]
  (let [parts (vec (str/split line #" "))
        [n from to] (map parse-long (mapv parts [1 3 5]))]
    [n (- from 1) (- to 1)]))

(defn parse [input]
  (let [[stacks cmds] (str/split input #"\n\n")]
    [(parse-stacks (drop-last 1 (str/split-lines stacks)))
     (map split-cmd (str/split-lines cmds))]))

(defn move-blocks [stacks n from-idx to-idx movef]
  (movef n (@stacks from-idx) (@stacks to-idx)))

(defn run [stacks cmds movef]
  (let [stacks (atom stacks)]
    (doseq [cmd cmds]
      (let [[n from-idx to-idx] cmd
            [from to] (move-blocks stacks n from-idx to-idx movef)]
        (swap! stacks assoc from-idx from)
        (swap! stacks assoc to-idx to)))
    @stacks))

(defn to-str [stacks]
  (apply str (map first stacks)))

(defn move-each [n from to]
  [(drop n from) (into to (take n from))])

(defn move-all [n from to]
  [(drop n from) (concat (take n from) to)])

(let [[stacks cmds] (parse (slurp "data/day5.input"))]
  (println (to-str (run stacks cmds move-each))) ; part 1
  (println (to-str (run stacks cmds move-all)))) ; part 2

