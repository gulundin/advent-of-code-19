(ns advent19.day5.core
  (:require [clojure.string :as str]))

(defn str->int-code [text]
  (->> (str/split text #",")
       (map read-string)
       (vec)))

(def input-data 5)

(def program
  (str->int-code (slurp "src/advent19/day5/input.txt")))

(defn read-addr [computer address]
  (get-in computer [:program address]))

(defn read-rel [computer offset]
  (read-addr computer (+ (get computer :pc) offset)))

(defn read-rel-pointer [computer offset]
  (read-addr computer (read-rel computer offset)))

(defn read-with-mode [computer offset mode]
  (case mode
    0 (read-rel-pointer computer offset)
    1 (read-rel computer offset)))

(defn write-addr [computer address value]
  (assoc-in computer [:program address] value))

(defn write-rel-pointer [computer offset value]
  (write-addr computer (read-rel computer offset) value))

(defn inc-pc [computer increment]
  (update computer :pc (partial + increment)))

(defn mode [modes n]
  (mod (nth (iterate #(quot % 10) modes) n) 10))

(defn binary-op [op computer modes]
  (let [res (op (read-with-mode computer 1 (mode modes 0))
                (read-with-mode computer 2 (mode modes 1)))]
    (inc-pc (write-rel-pointer computer 3 res) 4)))

(defn output [computer modes]
  (println (read-with-mode computer 1 (mode modes 0)))
  (inc-pc computer 2))

(defn input [computer]
  (inc-pc (write-rel-pointer computer 1 input-data) 2))

(defn jump [computer condition modes]
  (let [subject (read-with-mode computer 1 (mode modes 0))
        destination (read-with-mode computer 2 (mode modes 1))]
    (if (condition subject)
      (assoc computer :pc destination)
      (inc-pc computer 3))))

(defn stop [computer]
  (assoc computer :stopped true))

(defn op-code [instruction]
  (mod instruction 100))

(defn modes [instruction]
  (quot instruction 100))

(defn bool->int [b]
  (if b 1 0))

(defn execute-computer [initial-computer]
  (loop [computer initial-computer]
    (let [instruction (read-rel computer 0)
          modes (modes instruction)
          next-computer (case (op-code instruction)
                          1 (binary-op + computer modes)
                          2 (binary-op * computer modes)
                          3 (input computer)
                          4 (output computer modes)
                          5 (jump computer (comp not zero?) modes)
                          6 (jump computer zero? modes)
                          7 (binary-op (comp bool->int <) computer modes)
                          8 (binary-op (comp bool->int =) computer modes)
                          (stop computer))]
      (if (:stopped next-computer)
        next-computer
        (recur next-computer)))))

(defn execute []
  (execute-computer {:pc 0 :program program})
  ; Suppress result
  nil)