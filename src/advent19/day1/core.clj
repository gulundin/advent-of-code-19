(ns advent19.day1.core
  (:require [clojure.string :as str]))

(defn parse [path]
  (-> path
      slurp
      str/split-lines))

(defn fuel-for-module [mass]
  (-> mass
      (/ 3)
      Math/floor
      int
      (- 2)))

(def total-fuel
  (->> "src/advent19/day1/inputa.txt"
       parse
       (map (fn [s] (Integer/parseInt s)))
       (map fuel-for-module)
       (reduce + 0)))