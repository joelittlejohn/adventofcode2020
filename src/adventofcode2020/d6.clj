(ns adventofcode2020.d6
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (with-open [r (io/reader (io/resource "6.txt"))]
    (slurp r)))

(defn parse-groups
  [s]
  (->> (str/split-lines s)
       (partition-by #(= "" %))
       (remove #(= [""] %))))

(defn count-yes-from-any
  [g]
  (->> g (apply str) set count))

(->> input
     parse-groups
     (map count-yes-from-any)
     (apply +))
;;=> 6680






(defn count-yes-from-all
  [g]
  (->> g (map set) (apply set/intersection) count))

(->> input
     parse-groups
     (map count-yes-from-all)
     (apply +))
