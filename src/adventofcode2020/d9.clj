(ns adventofcode2020.d9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "9.txt"))]
    (->> r slurp str/split-lines (mapv #(Long/valueOf %)))))

(defn combinations
  [k l]
  (if (= 1 k) (map vector l)
      (reduce concat
              (map-indexed
               (fn [i x] (map #(cons x %) (combinations (dec k) (drop (inc i) l))))
               l))))

(defn valid?
  [xs]
  (get (->> xs butlast (combinations 2) (map (fn [[a b]](+ a b))) set) (last xs)))

(->> input
     (partition 26 1)
     (some #(when-not (valid? %) (last %))))
;;=> 257342611









(def target
  257342611)

(loop [acc 0 start 0 end 0]
  (cond (> acc target) (recur (- acc (input start)) (inc start) end)
        (< acc target) (recur (+ acc (input end)) start (inc end))
        (= acc target) (let [r (-> input (subvec start end) sort)]
                         (+ (first r) (last r)))))
;;=> 35602097
