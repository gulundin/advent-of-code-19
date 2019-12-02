(ns advent19.day2.core
  (:require [clojure.string :as str]))

(defn parse [path]
  (as-> path v
        (slurp v)
        (str/split v #",")
        (map read-string v)))

(def program
  (parse "src/advent19/day2/input.txt"))

(defn create-computer [program]
  {:pc 0 :program (vec program)})

(defn read-addr [computer address]
  (get-in computer [:program address]))

(defn read-rel [computer offset]
  (read-addr computer (+ (get computer :pc) offset)))

(defn write-addr [computer address value]
  (assoc-in computer [:program address] value))

(defn next-op [computer]
  (update computer :pc #(+ 4 %)))

(defn binary-op [op computer address1 address2 address3]
  (let [res (op (read-addr computer address1) (read-addr computer address2))]
    (next-op (write-addr computer address3 res))))

(defn execute-computer [initial-computer]
  (loop [computer initial-computer]
    (let [op (read-rel computer 0)
          addr1 (read-rel computer 1)
          addr2 (read-rel computer 2)
          addr3 (read-rel computer 3)]
      (case op
        1 (recur (binary-op + computer addr1 addr2 addr3))
        2 (recur (binary-op * computer addr1 addr2 addr3))
        computer))))

(defn execute [program noun verb]
  (-> program
      create-computer
      (write-addr 1 noun)
      (write-addr 2 verb)
      execute-computer
      (read-addr 0)))

(defn find-input [program expected-output]
  (let [mappings (for [noun (range 100) verb (range 100)]
                   {:noun noun, :verb verb, :result (execute program noun verb)})
        answers (filter #(= expected-output (:result %)) mappings)]
    (first answers)))