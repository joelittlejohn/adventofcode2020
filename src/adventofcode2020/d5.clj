(ns adventofcode2020.d4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (with-open [r (io/reader (io/resource "5.txt"))]
    (->> (slurp r) str/split-lines)))

(defn row
  [s]
  (Integer/parseInt (->> s (take 7) (replace {\F 0 \B 1}) (apply str)) 2))

(defn column
  [s]
  (Integer/parseInt (->> s (drop 7) (replace {\L 0 \R 1}) (apply str)) 2))

(defn seat-id
  [s]
  (-> (row s) (* 8) (+ (column s))))

(seat-id "FBFBBFFRLR") ;;=> 357
(seat-id "FFFBBBFRRR") ;;=> 119
(seat-id "BBFFBBFRLL") ;;=> 820

(->> input (map seat-id) sort last)
;;=> 987







(let [seat-ids (->> input (map seat-id) sort)
      missing-seat-ids (sort (set/difference (set (range 0 (inc (last seat-ids)))) (set seat-ids)))]
  (->> missing-seat-ids
       (partition 2 1)
       (some (fn [[s1 s2]] (when (> (- s2 s1) 1) s2)))))
;;=> 603
