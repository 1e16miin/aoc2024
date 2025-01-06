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

(defn find-valid-order
  [graph path visited order]
  (let [node (last path)
        candidates (->> (get graph node)
                        (remove visited))]
    (if (= (count path) (count order))
      path
      (some #(find-valid-order graph (conj path %) (conj visited %) order) candidates))))

(defn reorder
  [ordering-rules order]
  (let [order-set (set order)
        ordering-graph (->> ordering-rules
                            (filter (fn [rule] (every? order-set rule)))
                            (reduce (fn [graph [from to]]
                                      (update graph from conj to))
                                    {}))]
    (some #(find-valid-order ordering-graph [%] #{%} order) order)))

(defn solve-part2
  [file-name]
  (let [{:keys [ordering-rules orders]} (parse-input file-name)]
    (->> orders
         (remove #(valid-order? ordering-rules %))
         (map #(reorder ordering-rules %))
         (map middle-element)
         (apply +))))

(comment
  (solve-part2 "day5.txt"))
