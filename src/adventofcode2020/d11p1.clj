(ns adventofcode2020.d11p1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "11.txt"))]
    (->> r slurp str/split-lines (mapv vec))))

(defn available?
  [m [x y]]
  (= \L (get-in m [y x])))

(defn occupied?
  [m [x y]]
  (= \# (get-in m [y x])))

(defn count-adjacent
  [m [x y] pred]
  (->> (for [cx [-1 0 1]
             cy [-1 0 1]
             :let [[ax ay :as a] [(+ x cx) (+ y cy)]]
             :when (and (not= [0 0] [cx cy])
                        (< -1 ax (count (m y)))
                        (< -1 ay (count m)))]
         a)
       (filter #(pred m %))
       count))

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
    (and (available? m-old pos) (zero? (count-adjacent m-old pos occupied?)))
    (assoc-in m-new [y x] \#)

    (and (occupied? m-old pos) (>= (count-adjacent m-old pos occupied?) 4))
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
;;=> 2113
