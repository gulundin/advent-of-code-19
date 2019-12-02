(ns advent19.day2.core
  (:require [clojure.string :as str]
            [clojure.core.logic :as l]))

(defn set-input [noun verb program]
  (-> program
      (assoc 1 noun)
      (assoc 2 verb)))

(defn create-execution [program]
  {:pc 0 :program program})

(defn execute-step [{:keys [pc program]}]
  (let [op (get program pc)
        arg1 (get program (get program (+ pc 1)))
        arg2 (get program (get program (+ pc 2)))
        res-address (get program (+ pc 3))]
    (case op
      1 {:pc (+ pc 4), :program (assoc program res-address (+ arg1 arg2))}
      2 {:pc (+ pc 4), :program (assoc program res-address (* arg1 arg2))}
      99 {:stopped true, :program program})))

(defn execute-until-stopped [initial-state]
  (loop [state initial-state]
    (if (contains? state :stopped)
      (get state :program)
      (recur (execute-step state)))))

(defn execute [program noun verb]
  (->> program
       (set-input noun verb)
       create-execution
       execute-until-stopped
       first))

(defn parse [path]
  (vec (as-> path v
             (slurp v)
             (str/split v #",")
             (map read-string v))))

(def the-program
  (parse "src/advent19/day2/input.txt"))

(defn find-input [program expected-output]
  (let [mappings (for [noun (range 100) verb (range 100)]
                   {:noun noun, :verb verb, :result (execute program noun verb)})
        answers (filter #(= expected-output (:result %)) mappings)]
    (first answers)))