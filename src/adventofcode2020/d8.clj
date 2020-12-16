(ns adventofcode2020.d8
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "8.txt"))]
    (slurp r)))

(defn parse-instructions
  [s]
  (->> s str/split-lines (map #(str/split % #" ")) (mapv (fn [[i n]] [i (Integer/valueOf n)]))))

(defn execute
  [commands]
  (loop [n 0 acc 0 previous #{}]
    (let [[op v] (commands n)]
      (cond (previous n) acc
            (= "nop" op) (recur (inc n) acc (conj previous n))
            (= "acc" op) (recur (inc n) (+ acc v) (conj previous n))
            (= "jmp" op) (recur (+ n v) acc (conj previous n))))))

(->> input parse-instructions execute)
;;=> 1451







(defn safe-execute
  [commands]
  (loop [n 0 acc 0 previous #{}]
    (if (< n (count commands))
      (let [[op v] (commands n)]
        (cond (previous n) nil
              (= "nop" op) (recur (inc n) acc (conj previous n))
              (= "acc" op) (recur (inc n) (+ acc v) (conj previous n))
              (= "jmp" op) (recur (+ n v) acc (conj previous n))))
      acc)))

(let [instructions (parse-instructions input)
      correction {"jmp" "nop" "nop" "jmp"}
      possible-errors (->> (range 0 (count instructions))
                           (filter #(correction (get-in instructions [% 0]))))]
  (some #(safe-execute (update-in instructions [% 0] correction)) possible-errors))
