(ns advent19.day4.core)

(defn digits [n]
  "Converts a number to a seq of its digits"
  (->> (str n)
       (map #(Character/digit ^char % 10))))

(defn is-non-decreasing [numbers]
  (= numbers (sort numbers)))

(defn has-adjecent-pair [numbers]
  "Triplets and longer sequences does not count"
  (->> numbers
       (partition-by identity)
       (map count)
       (some (partial = 2))))

(defn answer []
  (->> (range 130254 678275)
       (map digits)
       (filter is-non-decreasing)
       (filter has-adjecent-pair)
       (count)))