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

(defn solve-part1
  [input]
  (count (let [board (create-board input)
               pattern "XMAS"]
           (for [x (range (:height board))
                 y (range (:width board))
                 direction [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
                 :when (and (= (get-in (:board board) [x y]) (first pattern))
                            (find-xmas-from board x y pattern direction))]
             [x y direction]))))


(defn find-mas-x
  [board x y]
  (let [positions (map (fn [direction]
                         (let [[dx dy] direction]
                           [[(+ x dx) (+ y dy)]]))
                       [[-1 -1] [-1 1] [1 -1] [1 1]])
        chars (map (fn [pos]
                     (map (fn [[nx ny]]
                            (when (in-bounds? nx ny board)
                              (get-in (:board board) [nx ny])))
                          pos))
                   positions)
        available-patterns ['(\M \S \M \S) '(\M \M \S \S) '(\S \S \M \M) '(\S \M \S \M)]]
    (some #(= (flatten chars) %) available-patterns)))

(defn solve-part2
  [input]
  (count (let [board (create-board input)]
           (for [x (range (:height board))
                 y (range (:width board))
                 :when (and (= (get-in (:board board) [x y]) \A)
                            (find-mas-x board x y))]
             [x y]))))

(comment
  (solve-part1 "day4.txt")
  (solve-part2 "day4.txt"))
