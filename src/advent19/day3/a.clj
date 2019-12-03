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
  ([instruction point] (move (:dir instruction) (:dist instruction) point))
  ([dir dist point]
   (let [[axis op] (case dir :R [:x #(+ % dist)]
                             :D [:y #(- % dist)]
                             :L [:x #(- % dist)]
                             :U [:y #(+ % dist)])]
     (update-in point [axis] op))))

(defn on-path
  ([instruction point] (on-path (:dir instruction) (:dist instruction) point))
  ([dir dist point]
   (->> (iterate inc 1)
        (map #(move dir % point))
        (take dist))))

(defn traverse-segment [points instruction]
   (let [current-point (or (last points) {:x 0 :y 0})]
     (into points (on-path instruction current-point))))

(defn traverse [instructions]
  ; Using a linked-list has huge impact on performance
  (reduce traverse-segment [] instructions))

(defn manhattan-distance [{:keys [x y]}]
  (+ (Math/abs ^int x) (Math/abs ^int y)))

(defn distance [point path]
  (inc (.indexOf path point)))

(defn answer []
  (let [[instructions1 instructions2] (parse "src/advent19/day3/input.txt")
        path1 (traverse instructions1)
        path2 (traverse instructions2)
        intersections (set/intersection (set path1) (set path2))]
    (apply min (map (fn [point] (+ (distance point path1)
                                   (distance point path2)))
                    intersections))))