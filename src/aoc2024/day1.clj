(ns aoc2024.day1
  (:require [util :refer [read-input]]))

(def input
  (read-input "day1.txt"))

(defn parse-input
  [input]
  (->> input
       (mapcat #(re-seq #"\d+" %))
       (map parse-long)
       (partition 2)
       (apply mapv vector)))

(defn solve-part1
  []
  (->> (parse-input input)
       (map sort)
       (apply map -)
       (map abs)
       (apply +)))

(defn calc-point
  [info id]
  (when-let [freq (get info id)]
    (* freq id)))

(defn solve-part2
  []
  (let [[left right] (parse-input input)
        right-freq-info (frequencies right)]
    (->> left
         (keep #(calc-point right-freq-info %))
         (apply +))))
