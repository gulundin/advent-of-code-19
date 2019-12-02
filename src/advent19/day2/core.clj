(ns advent19.day2.core
  (:require [clojure.string :as str]
            [clojure.core.logic :as l]))

(defn parse [path]
  (vec (as-> path v
             (slurp v)
             (str/split v #",")
             (map read-string v))))

(defn create-execution [program]
  {:pc 0 :program program})

(defn parse-op [opcode]
  (cond
    (= 1 opcode) {:op +, :args 3}
    (= 2 opcode) {:op *, :args 3}
    (= 99 opcode) {:args 0, :stopped true}))

(defn apply-op [op args program]
  (let [in-addresses (subvec args 0 (dec (count args)))
        in (map (fn [n] (nth program n)) in-addresses)
        res (apply op in)
        res-address (last args)]
    (assoc program res-address res)))

(defn execute-step [{:keys [pc program]}]
  (let [opcode (nth program pc)
        {op :op, args :args, stopped :stopped} (parse-op opcode)
        next-pc (+ pc args 1)
        args (subvec program (inc pc) next-pc)]
    (if stopped
      {:program program
       :stopped true}
      {:program (apply-op op args program)
       :pc      next-pc})))

(defn execute-until-stopped [initial-state]
  (loop [state initial-state]
    (if (contains? state :stopped)
      (:program state)
      (recur (execute-step state)))))

(defn set-input [noun verb program]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)))

(defn get-result [program]
  (first program))


(defn execute [program noun verb]
  (->> program
       (set-input noun verb)
       create-execution
       execute-until-stopped
       get-result))

(def the-program
  (parse "src/advent19/day2/input.txt"))

(defn find-input [program expected-output]
  (let [mappings (for [noun (range 100) verb (range 100)]
                   {:noun noun, :verb verb, :result (execute program noun verb)})
        answers (filter #(= expected-output (:result %)) mappings)]
    (first answers)))