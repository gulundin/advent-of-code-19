(ns advent19.day9.core
  (:require [clojure.string :as str]))

;(def input "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")
;(def input "1102,34915192,34915192,7,4,7,99,0")
;(def input "104,1125899906842624,99")
(def input (slurp "src/advent19/day9/input.txt"))

(defn str->int-code [text]
  (->> (str/split text #",")
       (map read-string)
       (vec)
       (reduce-kv assoc {})))

(def program
  (str->int-code input))

(defn read-addr [computer address]
  (get-in computer [:program address] 0))

(defn read-imm-mode [computer offset]
  (read-addr computer (+ (get computer :pc) offset)))

(defn read-pos-mode [computer offset]
  (read-addr computer (read-imm-mode computer offset)))

(defn read-rel-mode [computer offset]
  (read-addr computer (+ (:rel-base computer)
                         (read-imm-mode computer offset))))

(defn read-with-mode [computer offset mode]
  (case mode
    0 (read-pos-mode computer offset)
    1 (read-imm-mode computer offset)
    2 (read-rel-mode computer offset)))

(defn write-addr [computer address value]
  (assoc-in computer [:program address] value))

(defn write-pos-mode [computer offset value]
  (write-addr computer (read-imm-mode computer offset) value))

(defn write-rel-mode [computer offset value]
  (write-addr computer
              (+ (:rel-base computer)
                 (read-imm-mode computer offset))
              value))

(defn write-with-mode [computer offset value mode]
  (case mode
    0 (write-pos-mode computer offset value)
    2 (write-rel-mode computer offset value)))

(defn inc-pc [computer increment]
  (update computer :pc + increment))

(defn mode [modes n]
  (mod (nth (iterate #(quot % 10) modes) n) 10))

(defn binary-op [op computer modes]
  (let [res (op (read-with-mode computer 1 (mode modes 0))
                (read-with-mode computer 2 (mode modes 1)))]
    (inc-pc (write-with-mode computer 3 res (mode modes 2)) 4)))

(defn output [computer modes]
  (let [val (read-with-mode computer 1 (mode modes 0))]
    (println val)
    (-> computer
        (assoc :output val)
        (inc-pc 2))))

(defn input [computer modes]
  (let [input-data (get computer :input)
        new-computer (dissoc computer :phase)]
    (inc-pc (write-with-mode new-computer 1 input-data (mode modes 0)) 2)))

(defn jump [computer condition modes]
  (let [subject (read-with-mode computer 1 (mode modes 0))
        destination (read-with-mode computer 2 (mode modes 1))]
    (if (condition subject)
      (assoc computer :pc destination)
      (inc-pc computer 3))))

(defn adjust-rel-base [computer modes]
  (let [val (read-with-mode computer 1 modes)]
    (inc-pc (update computer :rel-base + val) 2)))

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
    (let [instruction (read-imm-mode computer 0)
          modes (modes instruction)
          next-computer (case (op-code instruction)
                          1 (binary-op + computer modes)
                          2 (binary-op * computer modes)
                          3 (input computer modes)
                          4 (output computer modes)
                          5 (jump computer (comp not zero?) modes)
                          6 (jump computer zero? modes)
                          7 (binary-op (comp bool->int <) computer modes)
                          8 (binary-op (comp bool->int =) computer modes)
                          9 (adjust-rel-base computer modes)
                          (stop computer))]
      (if (or (:stopped next-computer)
              (:yielding next-computer))
        (dissoc next-computer :yielding)
        (recur next-computer)))))

(defn make-computer [program input]
  {:pc 0 :program program :input input :rel-base 0})

(schedule (make-computer program 2))