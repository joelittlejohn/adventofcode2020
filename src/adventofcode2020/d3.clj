(ns adventofcode2020.d3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "3.txt"))]
    (->> (slurp r) str/split-lines (mapv cycle))))

(defn tree?
  [m [x y]]
  (= \# (nth (m y) x)))

(defn move
  [[x y] [vx vy]]
  [(+ x vx) (+ y vy)])

(defn count-trees
  [m [vx vy :as v]]
  (loop [c 0 [x y :as s] [0 0]]
    (if (>= y (count m))
      c
      (recur (if (tree? m s) (inc c) c) (move s v)))))

(count-trees input [3 1])
;;=> 234


(* (count-trees input [1 1])
   (count-trees input [3 1])
   (count-trees input [5 1])
   (count-trees input [7 1])
   (count-trees input [1 2]))
;; => 5813773056
