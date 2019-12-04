(ns advent19.day3.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def input (slurp "src/advent19/day3/input.txt"))

(defn parse-instruction [word]
  {:dir  (keyword (subs word 0 1))
   :dist (read-string (subs word 1))})

(defn parse-instructions [line]
  (->> (str/split line #",")
       (map parse-instruction)))

(defn parse [input]
  (map parse-instructions (str/split-lines input)))

(defn move-point [point dir dist]
  (let [[axis op] (case dir :R [:x #(+ % dist)]
                            :D [:y #(- % dist)]
                            :L [:x #(- % dist)]
                            :U [:y #(+ % dist)])]
    (update-in point [axis] op)))

(defn make-wire-segment [{:keys [dir dist]} point]
  (->> (iterate inc 1)
       (map (fn [distance] (move-point point dir distance)))
       (take dist)))

(defn add-wire-segment [wire instruction]
  (let [current-point (or (last wire) {:x 0 :y 0})]
    (into wire (make-wire-segment instruction current-point))))

(defn make-wire [instructions]
  (reduce add-wire-segment [] instructions))

(defn manhattan-distance [{:keys [x y]}]
  (+ (Math/abs ^int x) (Math/abs ^int y)))

(defn distance-along-wire [point path]
  (inc (.indexOf path point)))

(defn answer []
  (let [[instructions1 instructions2] (parse input)
        wire1 (make-wire instructions1)
        wire2 (make-wire instructions2)
        intersections (set/intersection (set wire1) (set wire2))]
    (apply min (map (fn [point] (+ (distance-along-wire point wire1)
                                   (distance-along-wire point wire2)))
                    intersections))))