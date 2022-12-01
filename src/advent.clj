(ns advent
  (:require [clojure.string :as str]))

(defn read-input
  [filename]
  (slurp (str "data/" filename)))

(defn lines
  [filename]
  (let [input (read-input filename)]
    (str/split-lines input)))

(defn groups
  [filename]
  (let [input (read-input filename)]
    (map str/split-lines
      (str/split input #"\n\n"))))
