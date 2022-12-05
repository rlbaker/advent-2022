(ns day5
  (:require [clojure.string :as str]))

(defn read-blocks [line]
  (mapv (vec line)
        (range 1 (count line) 4)))

(defn clean [col]
  (drop-while #(= \space %) col))

(defn parse-stacks [lines]
  (->> lines
       (map read-blocks)  ; gather all blocks from line
       (apply map list)   ; transpose the rows into stacks
       (map clean)        ; remove any \spaces from tops of stacks
       (zipmap (range)))) ; map of n -> stack

(defn split-cmd [line]
  (let [parts (vec (str/split line #" "))
        [n from to] (map parse-long (mapv parts [1 3 5]))]
    [n (- from 1) (- to 1)]))

(defn parse-cmds [lines]
  (map split-cmd lines))

(defn parse [input]
  (let [[stacks cmds] (str/split input #"\n\n")]
    [(parse-stacks (drop-last 1 (str/split-lines stacks)))
     (parse-cmds (str/split-lines cmds))]))

(def input (parse (slurp "data/day5.input")))

;;

(defn get-stacks [stacks cmd]
  (let [[n from to] cmd]
    [(get stacks from)
     (get stacks to)]))

(defn move [cmd stacks takef]
  (let [[n from-idx to-idx] cmd
        [from to] (get-stacks stacks cmd)]
    (merge stacks
           {from-idx (drop n from)
            to-idx (takef n from to)})))

(defn run [stacks cmds takef]
  (loop [stacks stacks
         cmds cmds]
    (if (empty? cmds)
      stacks
      (recur
        (move (first cmds) stacks takef)
        (rest cmds)))))

(defn to-str [stacks]
  (->> (sort stacks)
       (vals)
       (map first)
       (apply str)))

(defn by-one [n from to] (into to (take n from)))
(defn by-n [n from to] (concat (take n from) to))

(defn part1 [input]
  (let [[stacks cmds] input]
    (to-str (run stacks cmds by-one))))

(defn part2 [input]
  (let [[stacks cmds] input]
    (to-str (run stacks cmds by-n))))

(println (part1 input))
(println (part2 input))
