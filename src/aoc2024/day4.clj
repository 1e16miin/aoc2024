(ns aoc2024.day4
  (:require [util :refer [read-input]]))

(defn create-board
  [input]
  (let [board (->> (read-input input)
                   (mapv #(vec %)))]
    {:board board
     :height (count board)
     :width (count (first board))}))

(defn in-bounds?
  [x y board]
  (and (>= x 0) (< x (:height board))
       (>= y 0) (< y (:width board))))

(defn find-xmas-from
  [board x y path direction]
  (let [path' (rest path)]
    (if (empty? path')
      true
      (let [nx (+ x (first direction))
            ny (+ y (second direction))]
        (when (and (in-bounds? nx ny board)
                   (= (get-in (:board board) [nx ny]) (first path')))
          (find-xmas-from board nx ny path' direction))))))

(defn find-xmas
  [board]
  (let [pattern "XMAS"]
    (for [x (range (:height board))
          y (range (:width board))
          direction [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
          :when (and (= (get-in (:board board) [x y]) (first pattern))
                     (find-xmas-from board x y pattern direction))]
      [x y direction])))

(defn solve-part1
  [input]
  (let [board (create-board input)]
    (find-xmas board)))

(comment
  (count (solve-part1 "day4.txt")))
