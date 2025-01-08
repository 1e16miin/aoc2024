(ns aoc2024.day6
  (:require [util :refer [read-input]]))

(defn parse-grid [grid]
  (mapv vec grid))

(defn directions []
  [[-1 0] [0 1] [1 0] [0 -1]]) ; 위(0), 오른쪽(1), 아래(2), 왼쪽(3)

(defn in-bounds? [grid [x y]]
  (and (>= x 0) (< x (count grid))
       (>= y 0) (< y (count (grid 0)))))

(defn find-start [grid]
  (some (fn [[x row]]
          (some (fn [[y cell]]
                  (when (= cell \^)
                    [x y]))
                (map vector (range) row)))
        (map vector (range) grid)))

(defn next-state [grid [x y direction]]
  (let [directions (directions)
        [dx dy] (nth directions direction)
        nx (+ x dx)
        ny (+ y dy)]
    (if (in-bounds? grid [nx ny])
      (if (= \# (get-in grid [nx ny]))
        [x y (mod (inc direction) 4)] ; 오른쪽으로 90도 회전
        [nx ny direction]) ; 다음 위치로 이동
      nil))) ; 지도 밖으로 나가면 nil 반환

(defn find-escape-path [grid start-pos]
  (->> (iterate (fn [[x y direction]]
                  (next-state grid [x y direction]))
                (conj start-pos 0)) ; 시작 위치와 방향(위쪽) 추가
       (take-while some?) ; nil이 나올 때까지 반복
       (map (fn [[x y _]] [x y])))) ; 위치만 추출

(defn solve-part1 [file-name]
  (let [input (read-input file-name)
        parsed-grid (parse-grid input)
        start-pos (find-start parsed-grid)]
    (set (find-escape-path parsed-grid start-pos))))

(comment
  (count (solve-part1 "day6.txt")))
