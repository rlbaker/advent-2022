(ns advent (:require [clojure.string :as str]))

(defn split-groups [input] (map str/split-lines (str/split input #"\n\n")))
(defn split-fields [input] (map #(str/split % #" ") (str/split-lines input)))
