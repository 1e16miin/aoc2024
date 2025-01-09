(ns aoc2024.day7
  (:require [util :refer [read-input]]))

(defn parse-line
  [line]
  (->> (re-seq #"\d+" line)
       (map parse-long)))

(defn get-equations
  [file-name]
  (->> (read-input file-name)
       (map parse-line)))

(defn generate-operations
  [n]
  (if (= n 1)
    [[]]
    (let [ops [\* \+]]
      (->> (generate-operations (dec n))
           (mapcat (fn [seq]
                     (map #(conj seq %) ops)))))))

(defn calculate
  [[start & rests] operations]
  (reduce
   (fn [result [op number]]
     (if (= op \*)
       (* result number)
       (+ result number)))
   start
   (map vector operations rests)))

(defn equal?
  [[result & numbers]]
  (let [operations-sets (generate-operations (count numbers))
        calculate-results (map #(calculate numbers %) operations-sets)]
    (contains? (set calculate-results) result)))

(defn solve-part1
  [file-name]
  (->> (get-equations file-name)
       (filter equal?)
       (map first)
       (apply +)))

(comment
  (solve-part1 "day7.txt"))
