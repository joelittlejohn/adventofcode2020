(ns adventofcode2020.d14p1
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

(defn mask
  [m v]
  (let [bs (str/replace (format (str "%" (count m) "s") (Integer/toBinaryString v)) " " "0")]
    (-> (apply str (map (fn [b m] (if (= m \X) b m) ) bs m))
        (Long/parseLong 2))))

(def execute nil)
(defmulti execute
  (fn [_ instruction] (first instruction)))

(defmethod execute :mask
  [s [_ {:keys [bits]}]]
  (assoc s :mask (apply str bits)))

(defmethod execute :assignment
  [s [_ {:keys [location value]}]]
  (assoc-in s [:mem (Integer/valueOf (apply str (first location)))]
            (mask (:mask s) (Integer/valueOf (apply str value)))))

(mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11) ;;=> 73
(mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101) ;;=> 101
(mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 0) ;;=> 64

(->> (parse input)
     (map #(s/conform ::instruction (seq %)))
     (reduce execute {})
     :mem
     (map second)
     (reduce +))
;;=> 9967721333886
