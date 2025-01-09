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
  [n ops]
  (if (= n 1)
    [[]]
    (->> (generate-operations (dec n) ops)
         (mapcat (fn [seq]
                   (map #(conj seq %) ops))))))

(defn calculate
  [result [start & rests] operations]
  (reduce
   (fn [partial [op number]]
     (if (> partial result)
       (reduced partial)
       (case op
         "*" (* partial number)
         "+" (+ partial number)
         (parse-long (str partial number)))))
   start
   (map vector operations rests)))

(defn equal?
  [ops [result & numbers]]
  (let [operations-sets (generate-operations (count numbers) ops)
        calculate-results (map #(calculate result numbers %) operations-sets)]
    (contains? (set calculate-results) result)))

(defn solve-part1
  [file-name]
  (->> (get-equations file-name)
       (filter #(equal? ["*" "+"] %))
       (map first)
       (apply +)))

(defn solve-part2
  [file-name]
  (->> (get-equations file-name)
       (filter #(equal? ["||" "*" "+"] %))
       (map first)
       (apply +)))

(comment
  (solve-part2 "day7.txt"))
