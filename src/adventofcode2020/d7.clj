(ns adventofcode2020.d7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (with-open [r (io/reader (io/resource "7.txt"))]
    (-> r slurp str/split-lines)))

(defn parse-rule
  [s]
  (let [[_ colour] (re-find #"^(\w+ \w+) bags contain" s)
        contents (re-seq #"(\d) (\w+ \w+) bags?" s)]
    [colour (mapv (fn [[_ n c]] [(Integer/valueOf n) c]) contents)]))

(defn parse-rules
  [s]
  (->> input
       (map parse-rule)
       (into {})))

(defn contains-colours
  [colour rules]
  (set (reduce (fn [a [n c]] (concat (conj a c) (contains-colours c rules))) [] (rules colour))))

(let [rules (parse-rules input)]
  (->> (keys rules)
       (filter #(get (contains-colours % rules) "shiny gold"))
       count))
;;=> 252







(defn count-bags-inside
  [colour rules]
  (reduce (fn [a [n c]] (+ a n (* n (count-bags-inside c rules)))) 0 (rules colour)))

(let [rules (parse-rules input)]
  (count-bags-inside "shiny gold" rules))
;;=> 35487
