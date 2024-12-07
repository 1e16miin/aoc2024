(ns aoc2024.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input
  [filename]
  (-> filename
      io/resource
      slurp
      string/trim))

(defn multiply
  [text]
  (->> text
       (re-seq #"\d+")
       (map parse-long)
       (apply *)))

(defn find-pattern
  [sub-string]
  (->> sub-string
       (re-seq (re-pattern #"mul\((\d+),(\d+)\)"))
       (map first)))

(defn solve-part1
  [filename]
  (->> (read-input filename)
       (find-pattern)
       (map multiply)
       (apply +)))

(defn find-enabled
  [{:keys [multiplies enabled? sub-string]}]
  (if enabled?
    (let [target "don't()"
          idx (.indexOf sub-string target)
          multiplies' (if (>= idx 0)
                        (-> (subs sub-string 0 idx)
                            (find-pattern)
                            (concat multiplies))
                        (-> (find-pattern sub-string)
                            (concat multiplies)))
          sub-string' (when (>= idx 0)
                        (subs sub-string (+ idx (count target))))]
      {:multiplies multiplies'
       :sub-string sub-string'
       :enabled? false})
    (let [target "do()"
          idx (.indexOf sub-string target)
          sub-string' (when (>= idx 0)
                        (subs sub-string (+ idx (count target))))]
      {:multiplies multiplies
       :sub-string sub-string'
       :enabled? true})))

(defn solve-part2
  [filename]
  (let [init-string (read-input filename)
        multiplies (->> {:multiplies []
                         :sub-string init-string
                         :enabled? true}
                        (iterate find-enabled)
                        (drop-while :sub-string)
                        first
                        :multiplies)]
    (->> multiplies
         (map multiply)
         (apply +))))
