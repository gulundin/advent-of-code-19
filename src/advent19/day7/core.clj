(ns advent19.day7.core
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

; 139629729
;(def input "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5")
; 18216
;(def input "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")
(def input (slurp "src/advent19/day7/input.txt"))

(defn str->int-code [text]
  (->> (str/split text #",")
       (map read-string)
       (vec)))

(def program
  (str->int-code input))

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
  (let [val (read-with-mode computer 1 (mode modes 0))]
    (-> computer
        (assoc :output val)
        (assoc :yielding true)
        (inc-pc 2))))

(defn input [computer]
  (let [input-data (get computer :phase (get computer :input))
        new-computer (dissoc computer :phase)]
    (inc-pc (write-rel-pointer new-computer 1 input-data) 2)))

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

(defn schedule [input-computer]
  (loop [computer input-computer]
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
      (if (or (:stopped next-computer)
              (:yielding next-computer))
        (dissoc next-computer :yielding)
        (recur next-computer)))))

(defn all-stopped [computers]
  (->> computers
       (map :stopped)
       (remove true?)
       (empty?)))

(defn pipe-io [computers]
  (vec (let [ncomputers (count computers)]
         (for [i (range ncomputers)]
           (assoc (get computers i) :input (:output (get computers (mod (dec i) ncomputers))))))))

(defn execute-amplifiers [phase-settings]
  (let [ncomputers (count phase-settings)
        initial-computers (->> phase-settings
                               (map (fn [phase] {:pc 0 :program program :input 0 :phase phase}))
                               (vec))]
    (loop [computers initial-computers
           execute-next 0]
      (if (all-stopped computers)
        (:output (last computers))
        (recur (pipe-io (update-in computers [execute-next] schedule))
               (mod (inc execute-next) ncomputers))))))

(defn max-thrust []
  (->> (range 5 10)
       (combo/permutations)
       (map execute-amplifiers)
       (reduce max)))

(time (max-thrust))
