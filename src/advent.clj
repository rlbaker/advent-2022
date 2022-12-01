(ns advent
  (:require [clojure.string :as str]))

(defn read-input
  [filename]
  (slurp (str "data/" filename)))

(defn lines
  [filename]
  (str/split-lines
    (read-input filename)))

(defn groups
  [filename]
  (map str/split-lines
     (str/split (read-input filename) #"\n\n")))
