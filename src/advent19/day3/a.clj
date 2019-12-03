(ns advent19.day3.a
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-instruction [word]
  {:dir (keyword (subs word 0 1))
   :dist (read-string (subs word 1))})

(defn parse-instructions [line]
  (->> (str/split line #",")
       (map parse-instruction)))

(defn parse [path]
  (->> path
      slurp
      str/split-lines
      (map parse-instructions)))

(defn move
  "The point `dist` steps to the `dir` of `point`"
  ([instruction point] (move (:dir instruction) (:dist instruction) point))
  ([dir dist point]
   (case dir
     :R (update-in point [:x] (partial + dist))
     :D (update-in point [:y] #(- % dist))
     :L (update-in point [:x] #(- % dist))
     :U (update-in point [:y] (partial + dist)))))

(defn on-path
  "All points between `point` (not included) and the point `(move dir dist point)` (included)."
  ([instruction point] (on-path (:dir instruction) (:dist instruction) point))
  ([dir dist point]
   (->> (iterate inc 1)
        (map #(move dir % point))
        (take dist)
        (set))))

(defn traverse
  "All points encountered when traversing the path given by the `instructions`"
  ([instructions] (traverse instructions {:current-point {:x 0 :y 0} :points #{}}))
  ([instructions {:keys [current-point points]}]
   (if (empty? instructions)
     points
     (let [updated-points (set/union points (on-path (first instructions) current-point))
           next-point (move (first instructions) current-point)]
       (recur (rest instructions) {:current-point next-point :points updated-points})))))

(defn distance [{:keys [x y]}]
  "Manhattan distance to origo"
  (+ (Math/abs ^int x) (Math/abs ^int y)))

(defn answer []
  (let [[instructions1 instructions2] (parse "src/advent19/day3/input.txt")
        path1 (traverse instructions1)
        path2 (traverse instructions2)
        intersections (set/intersection path1 path2)]
    (apply min (map distance intersections))))