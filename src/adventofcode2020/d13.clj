(ns adventofcode2020.d13
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "13.txt"))]
    (-> r slurp str/split-lines)))

(defn parse
  [[t bs]]
  [(Integer/valueOf t) (->> (str/split bs #",") (remove #{"x"}) (map #(Integer/valueOf %)))])

(defn next-bus
  [t b]
  (->> (iterate #(+ b %) 0) (drop-while #(< % t)) first))

(let [[t bs] (parse input)
      [bus-id bus-time] (->> (zipmap bs (map #(next-bus t %) bs))
                             (sort-by second)
                             first)]
  (* bus-id (- bus-time t)))
;; [443 5]












(defn abs
  "arbitrary precision abs (works for BigInt)"
  [n]
  (max n (-' n)))

(defn egcd
  [a b]
  (cond (zero? a) [(abs b) 0 1]
        (zero? b) [(abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (abs b)
                     r0 (abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn chinese-remainder
  [n a]
  (let [product (apply * n)
        reducer (fn [sum [n_i a_i]]
                  (let [p (quot product n_i)
                        egcd (egcd p n_i)
                        inv_p (second egcd)]
                    (+ sum (* a_i inv_p p))))
        sum-prod (reduce reducer 0 (map vector n a))]
    (mod sum-prod product)))

(defn parse2
  [[_ bs]]
  (->> (str/split bs #",")
       (map-indexed (fn [i b] [b i]))
       (remove #(= "x" (first %)))
       (map (fn [[b i]] [(Integer/valueOf b) i]))))

(defn first-aligned
  [buses]
  {:post [(every? (fn [[b i]] (zero? (mod (+ i %) b))) buses)]}
  (long (chinese-remainder (map #(biginteger (first %)) buses)
                           (map (fn [[b i]] (biginteger (- b i))) buses))))

(first-aligned (parse2 ['_ "7,13,x,x,59,x,31,19"])) ;;=> 1068781
(first-aligned (parse2 ['_ "17,x,13,19"])) ;;=> 3417
(first-aligned (parse2 ['_ "67,7,59,61"])) ;;=> 754018
(first-aligned (parse2 ['_ "67,x,7,59,61"])) ;;=> 779210
(first-aligned (parse2 ['_ "67,7,x,59,61"])) ;;=> 1261476
(first-aligned (parse2 ['_ "1789,37,47,1889"])) ;;=> 1202161486
(first-aligned (parse2 input)) ;;=> 1058443396696792
