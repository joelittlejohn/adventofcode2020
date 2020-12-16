(ns adventofcode2020.d1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "1.txt"))]
    (->> (slurp r) str/split-lines (map #(Integer/valueOf %)))))

(defn combinations
  [k l]
  (if (= 1 k) (map vector l)
      (reduce concat
              (map-indexed
               (fn [i x] (map #(cons x %) (combinations (dec k) (drop (inc i) l))))
               l))))

(->> input
     (combinations 2)
     (some (fn [[a b :as p]] (when (= 2020 (+ a b)) p)))
     (apply *))
;; => 252724

(->> input
     (combinations 3)
     (some (fn [xs] (when (= 2020 (apply + xs)) xs)))
     (apply *))
;;  => 276912720
