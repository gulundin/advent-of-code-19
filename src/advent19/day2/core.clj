(ns advent19.day2.core
  (:require [clojure.string :as str]))

(defn parse [path]
  (as-> path v
        (slurp v)
        (str/split v #",")
        (map read-string v)
        (vec v)))

(def program
  (parse "src/advent19/day2/input.txt"))

(defn read-addr [computer address]
  (get-in computer [:program address]))

(defn read-rel [computer offset]
  (read-addr computer (+ (get computer :pc) offset)))

(defn read-rel-pointer [computer offset]
  (read-addr computer (read-rel computer offset)))

(defn write-addr [computer address value]
  (assoc-in computer [:program address] value))

(defn write-rel-pointer [computer offset value]
  (write-addr computer (read-rel computer offset) value))

(defn next-op [computer]
  (update computer :pc #(+ 4 %)))

(defn binary-op [op computer]
  (let [res (op (read-rel-pointer computer 1)
                (read-rel-pointer computer 2))]
    (next-op (write-rel-pointer computer 3 res))))

(defn execute-computer [initial-computer]
  (loop [computer initial-computer]
      (case (read-rel computer 0)
        1 (recur (binary-op + computer))
        2 (recur (binary-op * computer))
        computer)))

(defn execute [program noun verb]
  (-> {:pc 0 :program program}
      (write-addr 1 noun)
      (write-addr 2 verb)
      execute-computer
      (read-addr 0)))

(defn find-input [program expected-output]
  (let [mappings (for [noun (range 100) verb (range 100)]
                   {:noun noun, :verb verb, :result (execute program noun verb)})
        answers (filter #(= expected-output (:result %)) mappings)]
    (first answers)))