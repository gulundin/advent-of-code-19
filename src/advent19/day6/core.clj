(ns advent19.day6.core
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input (slurp "src/advent19/day6/input.txt"))

(def orbits
  (->> (str/split-lines input)
       (map #(str/split % #"\)"))
       (map (juxt second first))
       (into {})))

(def is-orbited-by
  (loop [o orbits
         orbited-by {}]
    (if-let [[k v] (first o)]
      (let [old-ks (or (get orbited-by v) #{})]
        (recur (dissoc o k) (assoc orbited-by v (conj old-ks k))))
      orbited-by)))

(def indirect-orbits
  (loop [planets [{:name "COM" :distance 0}]
         result 0]
    (if-let [planet (first planets)]
      (let [distance (:distance planet)
            orbiting (->> (get is-orbited-by (:name planet))
                          (map (fn [p] {:name p :distance (inc distance)})))]
        (recur (into (rest planets) orbiting)
               (+ result distance)))
      result)))

; Part 2
(def source (get orbits "YOU"))

(def dest (get orbits "SAN"))

(def all-planets (conj (keys orbits) "COM"))

(def planet-map
  (->> all-planets
       (map (fn [p] [p (conj (get is-orbited-by p) (get orbits p))]))
       (into {})))

(defn distance [s d]
  (loop [planets [{:name s :distance 0}]
         visited #{}]
    (let [planet (first planets)
          neighbours (->> (get planet-map (:name planet))
                          (map (fn [p] {:name p :distance (inc (:distance planet))})))]
      (if (= (:name planet) d)
        (:distance planet)
        (if (contains? visited (:name planet))
          (recur (rest planets) visited)
          (recur (into (rest planets) neighbours) (conj visited (:name planet))))))))