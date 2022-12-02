(ns advent
  (:require [clojure.string :as str]))

(defn read-input
  [filename]
  (slurp (str "data/" filename)))

(defn split-fields [s] (str/split s #" "))
(defn split-groups [s] (str/split s #"\n\n"))

(defn lines [filename]
  (str/split-lines
    (read-input filename)))

(defn groups [filename]
  (map str/split-lines
       (split-groups (read-input filename))))

(defn matrix [filename]
  (map split-fields
       (lines filename)))
