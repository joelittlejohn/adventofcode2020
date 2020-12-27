(ns adventofcode2020.d16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "16.txt"))]
    (->> (slurp r) str/split-lines)))

(defn parse-rule
  [s]
  (let [[_ field a1 a2 b1 b2] (re-find #"(.+): (\d+)-(\d+) or (\d+)-(\d+)" s)
        a1 (Integer/valueOf a1)
        a2 (Integer/valueOf a2)
        b1 (Integer/valueOf b1)
        b2 (Integer/valueOf b2)]
    [field #(or (<= a1 % a2) (<= b1 % b2))]))

(defn parse-rules
  [lines]
  (->> lines (take-while #(not= "" %)) (map parse-rule) (into {})))

(defn parse-ticket
  [s]
  (map #(Integer/valueOf %) (str/split s #",")))

(let [rules (->> input parse-rules vals)
      valid? (apply some-fn rules)]
  (->> input
       (drop-while #(not= "nearby tickets:" %))
       next
       (mapcat parse-ticket)
       (remove valid?)
       (reduce +)))
;;=> 27911





(defn resolve-fields
  [column-fields]
  (if-let [f (some #(when (and (coll? %) (= 1 (count %))) (first %)) column-fields)]
    (->> column-fields (map #(cond (= #{f} %) f (set? %) (disj % f) :else %)) resolve-fields)
    column-fields))


(let [rules (parse-rules input)
      valid? (apply some-fn (vals rules))
      my-ticket (->> input (drop-while #(not= "your ticket:" %)) second parse-ticket)
      nearby-tickets (->> input (drop-while #(not= "nearby tickets:" %)) next (map parse-ticket))
      valid-tickets (filter #(every? valid? %) nearby-tickets)
      columns (apply mapv list valid-tickets)]
  (->> columns
       (map (fn [c] (set (keep (fn [[field rule-fn]] (when (every? rule-fn c) field)) rules))))
       resolve-fields
       (zipmap my-ticket)
       (keep (fn [[v f]] (when (.startsWith f "departure") v)))
       (reduce *)))
;;=> 737176602479
