(ns adventofcode2020.d17p2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "17.txt"))]
    (->> (slurp r) str/split-lines (mapv vec))))

(def test
  (->> ".#.
..#
###" str/split-lines (mapv vec)))

(def ACTIVE \#)
(def INACTIVE \.)

(defn active?
  [u pos]
  (= ACTIVE (get-in u [:cubes pos])))

(defn index
  [init]
  (let [xs (range (count (first (first (first init)))))
        ys (range (count (first (first init))))
        zs (range (count (first init)))
        ws (range (count init))]
    {:cubes (into {} (for [x xs y ys z zs w ws] [[x y z w] (get-in init [w z y x])]))
     :xs xs
     :ys ys
     :zs zs
     :ws ws}))

(defn extend
  [u]
  (let [f #(concat [(dec (first %))] % [(inc (last %))])]
    (-> u
        (update :xs f)
        (update :ys f)
        (update :zs f)
        (update :ws f))))

(defn neighbours
  [[x y z w]]
  (for [nx (range (dec x) (+ 2 x))
        ny (range (dec y) (+ 2 y))
        nz (range (dec z) (+ 2 z))
        nw (range (dec w) (+ 2 w))
        :when (not= [x y z w] [nx ny nz nw])]
    [nx ny nz nw]))

(defn positions
  [{:keys [xs ys zs ws]}]
  (for [x xs y ys z zs w ws] [x y z w]))

(defn new-cube-state
  [u pos]
  (let [c (->> (neighbours pos) (filter #(active? u %)) count)]
    (cond (and (active? u pos) (#{2 3} c))
          ACTIVE
          (= 3 c)
          ACTIVE
          :else
          INACTIVE)))

(defn step
  [u]
  (let [u (extend u)]
    (reduce #(assoc-in %1 [:cubes %2] (new-cube-state u %2)) u (positions u))))

(defn count-active
  [u]
  (->> u :cubes (filter (fn [[p v]] (= ACTIVE v))) count))

(-> [[input]]
    index
    step
    step
    step
    step
    step
    step
    count-active)
;;=> 2676
