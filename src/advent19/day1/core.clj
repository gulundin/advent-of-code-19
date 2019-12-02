(ns advent19.day1.core
  (:require [clojure.java.io :as io]
            [clojure.core.async :as a]))

(defn fuel-for-weight [mass]
  "Does not take the weight of the fuel itself into account"
  (-> mass
      (quot 3)
      (- 2)))

(def total-fuel-for-weight
  "Accounts for the weight of the fuel itself"
  ; memoization is thread-safe, but I suspect that thread-local memoization would be faster
  (memoize (fn [mass]
             (if (<= mass 0)
               0
               (let [fuel (fuel-for-weight mass)]
                 (+ fuel (total-fuel-for-weight fuel)))))))

(defn total-fuel []
  (let [x-form (comp
                 (map #(Integer/parseInt %))
                 (map total-fuel-for-weight))]
    (with-open [reader (io/reader "src/advent19/day1/input.txt")]
      (transduce x-form + (line-seq reader)))))

(defn total-fuel-parallel []
  (let [x-form (comp
                 (map #(Integer/parseInt %))
                 (map total-fuel-for-weight))]
    (with-open [reader (io/reader "src/advent19/day1/input.txt")]
      (let [n-processors (.availableProcessors (Runtime/getRuntime))
            lines (line-seq reader)
            in (a/to-chan lines)
            out (a/chan)]
        (a/pipeline n-processors out x-form in)
        (a/<!! (a/reduce + 0 out))))))