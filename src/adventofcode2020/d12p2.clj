(ns adventofcode2020.d12p2
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

(defn difference
  [[x1 y1] [x2 y2]]
  [(- x1 x2) (- y1 y2)])

(defn rotate-right
  [[x y] degrees]
  (case (mod degrees 360)
    0   [x y]
    90  [(- y) x]
    180 [(- x) (- y)]
    270 [y (- x)]))

(defn move
  [s [c v]]
  (case c
    \N (update-in s [:waypoint 1] #(- % v))
    \S (update-in s [:waypoint 1] #(+ % v))
    \E (update-in s [:waypoint 0] #(+ % v))
    \W (update-in s [:waypoint 0] #(- % v))
    \R (let [d (-> (difference (:waypoint s) (:ship s))
                   (rotate-right v))]
         (assoc s :waypoint (plus (:ship s) d)))
    \L (let [d (-> (difference (:waypoint s) (:ship s))
                   (rotate-right (- 360 v)))]
         (assoc s :waypoint (plus (:ship s) d)))
    \F (let [d (difference (:waypoint s) (:ship s))
             ship (nth (iterate #(plus d %) (:ship s)) v)
             waypoint (plus ship d)]
         (assoc s :ship ship :waypoint waypoint))))

(defn manhattan
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(->> input
     parse-commands
     (reduce move {:ship [0 0] :waypoint [10 -1]})
     :ship
     manhattan)
;;=> 51249
