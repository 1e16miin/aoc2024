(ns aoc2024.day6
  (:require [util :refer [read-input]]))

(defn find-coordinates
  [target height flatten-input]
  (keep-indexed (fn [idx val]
                  (when (= val target)
                    (let [x (rem idx height)
                          y (quot idx height)]
                      [x y])))
                flatten-input))

(defn get-board
  [file-name]
  (let [input (read-input file-name)
        height (count input)
        width (count (first input))
        flatten-input (->> input
                           (apply str)
                           seq)
        obstructions (find-coordinates \# height flatten-input)
        start-point (first (find-coordinates \^ height flatten-input))]
    {:obstructions (set obstructions)
     :point start-point
     :height height
     :width width
     :direction :u
     :path []}))

(def direction-relation
  {:u :r
   :r :d
   :d :l
   :l :u})

(defn in-range?
  [{:keys [point height width]}]
  (let [[x y] point]
    (and (< 0 x width) (< 0 y height))))

(defn next-point
  [direction point]
  (case direction
    :u (mapv + point [0 -1])
    :r (mapv + point [1 0])
    :d (mapv + point [0 1])
    :l (mapv + point [-1 0])))

(defn exit
  [{:keys [point obstructions direction path] :as board}]
  (let [path' (conj path point)
        point' (next-point direction point)
        obstruction? (contains? obstructions point')
        direction' (if obstruction?
                     (direction-relation direction)
                     direction)
        next-point (if obstruction?
                     (next-point direction' point)
                     point')]
    (-> board
        (assoc :point next-point)
        (assoc :direction direction')
        (assoc :path path'))))

(defn solve-part1
  [file-name]
  (let [board (get-board file-name)
        final-state (->> (iterate exit board)
                         (drop-while in-range?)
                         first)]
    (-> final-state
        :path
        set
        count)))

(comment
  (solve-part1 "day6.txt")
  (get-board "day6_sample.txt"))
