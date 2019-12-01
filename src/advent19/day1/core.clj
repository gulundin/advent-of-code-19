(ns advent19.day1.core
  (:require [clojure.java.io :as io]))

(defn fuel-for-weight [mass]
  "Does not take the weight of the fuel itself into account"
  (-> mass
      (quot 3)
      (- 2)))

(def total-fuel-for-weight
  "Accounts for the weight of the fuel itself"
  (memoize (fn [mass]
             (if (<= mass 0)
               0
               (let [fuel (fuel-for-weight mass)]
                 (+ fuel (total-fuel-for-weight fuel)))))))

(defn total-fuel []
  (with-open [reader (io/reader "src/advent19/day1/input.txt")]
    (->> (line-seq reader)
         (map #(Integer/parseInt %))
         (map total-fuel-for-weight)
         (reduce +))))