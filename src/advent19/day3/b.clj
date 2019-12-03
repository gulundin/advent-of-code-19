(ns advent19.day3.b
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-instruction [word]
  {:dir  (keyword (subs word 0 1))
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
   (let [[axis op] (case dir :R [:x #(+ % dist)]
                             :D [:y #(- % dist)]
                             :L [:x #(- % dist)]
                             :U [:y #(+ % dist)])]
     (update-in (update-in point [:dist] (partial + dist)) [axis] op))))

(defn on-path
  "All points between `point` (not included) and the point `(move dir dist point)` (included)."
  ([instruction point] (on-path (:dir instruction) (:dist instruction) point))
  ([dir dist point]
   (->> (iterate inc 1)
        (map #(move dir % point))
        (take dist))))

(defn traverse-segment [points instruction]
  "All points encountered while following `instruction` from the last point in `points`,
   plus `points`.

   Meant to be used with `reduce`."
  (let [current-point (or (first points) {:x 0 :y 0 :dist 0})]
    (into points (on-path instruction current-point))))

(defn drop-distances [points]
  "Removes the `:dist` key from every point in `points`"
  (map #(dissoc % :dist) points))

(defn calc-distances [points]
  "Converts a seq of `points` (including a `:dist`) to a map
   from the point (without `:dist`) to the shortest `:dist` that
   occurred for that point in the input seq."
  (let [grouped (group-by #(dissoc % :dist) points)]
    (into {} (for [[point full-points] grouped]
               [point (apply min (map :dist full-points))]))))

(defn traverse [instructions]
  "All points encountered when traversing the path given by the `instructions`"
  ; Using a linked-list has huge impact on performance
  (let [points (reduce traverse-segment '() instructions)]
    {:points (drop-distances points) :distances (calc-distances points)}))

(defn distance [{:keys [x y]}]
  "Manhattan distance to origo"
  (+ (Math/abs ^int x) (Math/abs ^int y)))

(defn answer []
  (let [[instructions1 instructions2] (parse "src/advent19/day3/input.txt")
        {path1 :points distances1 :distances} (traverse instructions1)
        {path2 :points distances2 :distances} (traverse instructions2)
        intersections (set/intersection (set path1) (set path2))
        distances (map #(+ (get distances1 %)
                           (get distances2 %))
                       intersections)]
    (apply min distances)))