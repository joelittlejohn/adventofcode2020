(ns adventofcode2020.d12p1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "12.txt"))]
    (slurp r)))

(defn parse-commands
  [s]
  (->> s str/split-lines (map (fn [[c & v]] [c (Integer/valueOf (apply str v))]))))

(def clockwise
  (cycle [[0 -1] [1 0] [0 1] [-1 0]]))

(def anticlockwise
  (->> clockwise (take 4) reverse cycle))

(defn plus
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn move
  [s [c v]]
  (case c
    \N (update-in s [:position 1] #(- % v))
    \S (update-in s [:position 1] #(+ % v))
    \E (update-in s [:position 0] #(+ % v))
    \W (update-in s [:position 0] #(- % v))
    \R (update s :direction (fn [d] (->> clockwise (drop-while #(not= d %)) (drop (/ v 90)) first)))
    \L (update s :direction (fn [d] (->> anticlockwise (drop-while #(not= d %)) (drop (/ v 90)) first)))
    \F (update s :position (fn [p] (nth (iterate #(plus (:direction s) %) p) v)))))

(defn manhattan
  [{:keys [position]}]
  (+ (Math/abs (first position)) (Math/abs (second position))))

(->> input
     parse-commands
     (reduce move {:direction [1 0] :position [0 0]})
     manhattan)

;;=> 757
