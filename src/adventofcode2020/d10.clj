(ns adventofcode2020.d10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "10.txt"))]
    (->> r slurp str/split-lines (mapv #(Integer/valueOf %)))))

(let [differences (->> input
                       sort
                       (cons 0) ;; the outlet
                       (partition 2 1)
                       (map (fn [[a b]] (- b a)))
                       (cons 3) ;; the device
                       (frequencies))]
  (* (differences 1) (differences 3)))
;;=> 2048








(defn adapter-tree
  [adapters]
  (->> (for [a (cons 0 adapters)
             b (filter #(and (> % a) (<= % (+ a 3))) adapters)]
         {a [b]})
       (apply merge-with into)))

(def n-paths
  (memoize
   (fn [m k]
     (if-let [children (m k)]
       (reduce #(+ %1 (n-paths m %2)) 0 children)
       1))))

(-> input
    adapter-tree
    (n-paths 0))
;;=> 1322306994176
