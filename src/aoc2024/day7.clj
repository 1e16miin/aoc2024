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

(defn find-valid-operations
  [numbers target current-value rest-numbers]
  (if (empty? rest-numbers)
    (= current-value target)
    (let [next-num (first rest-numbers)
          remaining (rest rest-numbers)]
      (or (find-valid-operations numbers target
                                 (+ current-value next-num)
                                 remaining)
          (find-valid-operations numbers target
                                 (* current-value next-num)
                                 remaining)
          ;; for part2
          (find-valid-operations numbers target
                                 (parse-long (str current-value next-num))
                                 remaining)))))

(defn equal?
  [[result & numbers]]
  (find-valid-operations numbers result (first numbers) (rest numbers)))

(defn solve
  [file-name]
  (->> (get-equations file-name)
       (filter equal?)
       (map first)
       (apply +)))

(comment
  (solve "day7.txt"))
