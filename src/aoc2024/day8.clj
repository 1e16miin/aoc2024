(ns aoc2024.day8
  (:require [util :refer [read-input]]))

(defn parse-grid
  [file-name]
  (let [grid (->> (read-input file-name)
                  (mapv vec))]
    (for [x (range (count grid))
          y (range (count (first grid)))]
      {:frequency (get-in grid [x y])
       :position [x y]})))

(defn mid-point?
  [[x1 y1] [[x2 y2] [x3 y3]]]
  (or (and (= (/ (+ x1 x3) 2) x2)
           (= (/ (+ y1 y3) 2) y2))
      (and (= (/ (+ x1 x2) 2) x3)
           (= (/ (+ y1 y2) 2) y3))))

(defn anti-node
  [antenna-pairs node]
  (some #(mid-point? node %) antenna-pairs))

(defn calculate-antinodes
  "안테나의 위치와 주파수를 기반으로 안티노드를 계산합니다."
  [grid]
  (let [antennas (remove #(= \. (:frequency %)) grid)
        nodes (->> grid
                   (map :position))
        antenna-pairs (for [a antennas
                            b antennas
                            :when (and (= (:frequency a) (:frequency b))
                                       (not= (:position a) (:position b)))]
                        [(:position a) (:position b)])]
    (filter #(anti-node antenna-pairs %) nodes)))

(defn solve-part1
  [file-name]
  (let [grid (parse-grid file-name)
        antinodes (calculate-antinodes grid)]
    (count antinodes)))

(comment
  (solve-part1 "day8.txt"))
