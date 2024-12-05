(ns util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn read-input
  [filename]
  (-> filename
      io/resource
      slurp
      string/trim
      (string/split #"\n")))
