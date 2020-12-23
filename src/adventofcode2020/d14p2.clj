(ns adventofcode2020.d14p2
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "14.txt"))]
    (slurp r)))

(s/def ::number
  (s/+ (set "0123456789")))

(s/def ::mask
  (s/cat :m #{\m} :a #{\a} :s #{\s} :k #{\k} :space #{\space} :eq #{\=} :space #{\space} :bits (s/+ #{\1\0\X})))

(s/def ::assignment
  (s/cat :m #{\m} :e #{\e} :m #{\m} :lsq #{\[} :location (s/+ ::number) :rsq #{\]} :space #{\space} :eq #{\=} :space #{\space} :value ::number))

(s/def ::instruction
  (s/or :mask ::mask :assignment ::assignment))

(defn parse
  [s]
  (->> s (str/split-lines)))

(defn addresses
  [l m]
  (let [bs (str/replace (format (str "%" (count m) "s") (Integer/toBinaryString l)) " " "0")
        updated (map (fn [cb cm] (case cm \0 [cb] \1 [\1] \X [\0 \1])) bs m)]
    (for [ _0 (nth updated 0)   _1 (nth updated 1)   _2 (nth updated 2)   _3 (nth updated 3)
           _4 (nth updated 4)   _5 (nth updated 5)   _6 (nth updated 6)   _7 (nth updated 7)
           _8 (nth updated 8)   _9 (nth updated 9)  _10 (nth updated 10) _11 (nth updated 11)
          _12 (nth updated 12) _13 (nth updated 13) _14 (nth updated 14) _15 (nth updated 15)
          _16 (nth updated 16) _17 (nth updated 17) _18 (nth updated 18) _19 (nth updated 19)
          _20 (nth updated 20) _21 (nth updated 21) _22 (nth updated 22) _23 (nth updated 23)
          _24 (nth updated 24) _25 (nth updated 25) _26 (nth updated 26) _27 (nth updated 27)
          _28 (nth updated 28) _29 (nth updated 29) _30 (nth updated 30) _31 (nth updated 31)
          _32 (nth updated 32) _33 (nth updated 33) _34 (nth updated 34) _35 (nth updated 35)]
      (Long/parseLong (str  _0  _1  _2  _3  _4  _5  _6  _7  _8  _9 _10 _11
                           _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22 _23
                           _24 _25 _26 _27 _28 _29 _30 _31 _32 _33 _34 _35) 2))))

(def execute nil)
(defmulti execute
  (fn [_ instruction] (first instruction)))

(defmethod execute :mask
  [s [_ {:keys [bits]}]]
  (assoc s :mask (apply str bits)))

(defmethod execute :assignment
  [s [_ {:keys [location value]}]]
  (let [v (Integer/valueOf (apply str value))
        l (Integer/valueOf (apply str (first location)))]
    (reduce #(assoc-in %1 [:mem %2] v) s (addresses l (:mask s)))))

(->> (parse input)
     (map #(s/conform ::instruction (seq %)))
     (reduce execute {})
     :mem
     (map second)
     (reduce +))
;;=> 4355897790573
