(ns adventofcode2020.d11p2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "11.txt"))]
    (->> r slurp str/split-lines (mapv vec))))

(def directions
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :let [d [dx dy]]
        :when (not= [0 0] d)]
    d))

(defn available?
  [m [x y]]
  (= \L (get-in m [y x])))

(defn occupied?
  [m [x y]]
  (= \# (get-in m [y x])))

(defn seat?
  [m [x y]]
  ({\L \#} (get-in m [y x])))

(defn empty?
  [m [x y]]
  (= \. (get-in m [y x])))

(defn plus
  [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn visible-occupied?
  [m pos d]
  (let [h (count m)
        w (count (m 0))
        path (->> (iterate #(plus d %) pos)
                  next
                  (take-while (fn [[x y]] (and (>= x 0) (>= y 0) (< x w) (< y h)))))]
    (some->> path (drop-while #(empty? m %)) first (occupied? m))))

(defn count-visible-occupied?
  [m pos]
  (reduce #(if (visible-occupied? m pos %2) (inc %1) %1) 0 directions))

(defn positions
  [m]
  (for [x (range (count (m 0))) y (range (count m))]
    [x y]))

(defn count-occupied
  [m]
  (->> (positions m)
       (filter #(occupied? m %))
       count))

(defn update-seat
  [m-old m-new [x y :as pos]]
  (cond
    (and (available? m-old pos) (zero? (count-visible-occupied? m-old pos)))
    (assoc-in m-new [y x] \#)

    (and (occupied? m-old pos) (>= (count-visible-occupied? m-old pos) 5))
    (assoc-in m-new [y x] \L)

    :else m-new))

(defn update-seats
  [m]
  (reduce #(update-seat m %1 %2) m (positions m)))

(defn stabilize
  [m]
  (let [m2 (update-seats m)]
    (if (= m m2)
      m2
      (recur m2))))

(-> (stabilize input)
    count-occupied)

;;=> 1865
