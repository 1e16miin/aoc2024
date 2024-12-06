(ns aoc2024.day2
  (:require [util :refer [read-input]]))

(def input
  (read-input "day2.txt"))

(defn ->level
  [line]
  (->> line
       (re-seq #"\d+")
       (map parse-long)))

(defn parse-input
  [input]
  (map ->level input))

(defn pairwise
  [seq]
  (->> seq
       (partition 2 1 seq)
       drop-last))

(defn safe?
  [pairs]
  (let [diffs (->> pairs
                   (map #(apply - %))
                   (into #{}))]
    (or (every? #{-3 -2 -1} diffs)
        (every? #{3 2 1} diffs))))

(defn solve-part1
  []
  (->> (parse-input input)
       (map pairwise)
       (filter safe?)
       count))

(defn drop-nth
  [n coll]
  (concat (take (dec n) coll) (drop n coll)))

(defn all-seq
  [seq]
  (for [n (range (inc (count seq)))]
    (drop-nth n seq)))

(defn safe?-2
  [seqs]
  (->> seqs
       (map pairwise)
       (some safe?)))

(defn solve-part2
  []
  (->> (parse-input input)
       (map all-seq)
       (filter safe?-2)
       count))
