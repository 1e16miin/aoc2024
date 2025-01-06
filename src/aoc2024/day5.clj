(ns aoc2024.day5
  (:require [util :refer [read-input]]))

(defn parse-input
  [file-name]
  (let [input (read-input file-name)
        [section1 _ section2] (partition-by #(= "" %) input)
        ordering-rules (->> section1
                            (map #(re-matches #"(\d+)\|(\d+)" %))
                            (map (fn [[_ before after]] [before after]))
                            set)
        orders (map #(re-seq #"\d+" %) section2)]
    {:ordering-rules ordering-rules
     :orders orders}))

(defn valid-order?
  [ordering-rules order]
  (every? ordering-rules (partition 2 1 order)))

(defn middle-element
  [lst]
  (let [len (count lst)
        mid-index (quot len 2)]
    (parse-long (nth lst mid-index))))

(defn solve-part1
  [file-name]
  (let [{:keys [ordering-rules orders]} (parse-input file-name)]
    (->> orders
         (filter #(valid-order? ordering-rules %))
         (map middle-element)
         (apply +))))

(comment
  (solve-part1 "day5.txt"))
