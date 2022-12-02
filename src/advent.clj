(ns advent (:require [clojure.string :as str]))

(defn split-fields [s] (str/split s #" "))
(defn split-groups [s] (str/split s #"\n\n"))
(defn lines [input] (str/split-lines input))
(defn groups [input] (map str/split-lines (split-groups input)))
(defn fields [input] (map split-fields (str/split-lines input)))
