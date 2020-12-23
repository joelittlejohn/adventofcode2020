(ns adventofcode2020.d15
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def input
  [9 19 1 6 0 5 4])

(defn say
  [s]
  (let [last-said (:say s)
        n (:n s)
        next (if-let [pos (get-in s [:previous last-said])]
               (- n pos)
               0)]
    (-> s
        (assoc :say next :n (inc n))
        (update :previous assoc! last-said n))))

(defn turns
  [init n]
  {:pre [(>= n (count init))]}
  (:say
   (reduce
    (fn [s _] (say s))
    {:say (last init)
     :n (dec (count init))
     :previous (->> init butlast (map-indexed (fn [i x] [x i])) (into {}) transient)}
    (range (- n (count init))))))

(turns input 2020)
;;=> 1522


(turns input 30000000)
;;=> 18234
