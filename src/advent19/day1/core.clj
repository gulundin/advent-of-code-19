(ns advent19.day1.core
  (:require [clojure.string :as str]))

(defn parse [path]
  "Into an int seq"
  (->> path
      slurp
      str/split-lines
      (map (fn [s] (Integer/parseInt s)))))

(defn fuel-for-weight [mass]
  "Does not take the weight of the fuel itself into account"
  (-> mass
      (/ 3)
      Math/floor
      int
      (- 2)))

(defn fuel-for-module-of-weight [m]
  "Accounts for the weight of the fuel itself"
  (loop [total-fuel 0
         new-fuel (fuel-for-weight m)]
    (if (<= new-fuel 0)
      total-fuel
      (recur (+ total-fuel new-fuel) (fuel-for-weight new-fuel)))))

(def total-fuel
  (->> "src/advent19/day1/input.txt"
       parse
       (map fuel-for-module-of-weight)
       (reduce +)))