(ns advent19.day8.core
  (:require [clojure.string :as str]))

(def input (slurp "src/advent19/day8/input.txt"))
(def width 25)
(def height 6)
(def size (* width height))

(defn n-chars [char coll]
  (count (filter (partial = char) coll)))

(defn answer1 []
  (->> input
       (partition size input)
       (apply min-key (partial n-chars \0))
       (#(* (n-chars \1 %) (n-chars \2 %)))))

(defn answer2 []
  (->> input
       (partition size)
       (apply map vector)
       (map #(first (filter (partial not= \2) %)))
       (map {\0 \  \1 \#})
       (partition width)
       (map str/join)))