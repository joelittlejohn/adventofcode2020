(ns adventofcode2020.d4
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

(def input
  (with-open [r (io/reader (io/resource "4.txt"))]
    (slurp r)))

(defn parse-passport
  [s]
  (->> (str/split s #" ")
       (map #(str/split % #":"))
       (into {})
       walk/keywordize-keys))

(defn parse-passports
  [s]
  (->> (str/split-lines s)
       (partition-by #(= "" %))
       (remove #(= [""] %))
       (map #(str/join " " %))
       (map parse-passport)))

(def fields
  #{:byr; (Birth Year)
    :iyr; (Issue Year)
    :eyr; (Expiration Year)
    :hgt; (Height)
    :hcl; (Hair Color)
    :ecl; (Eye Color)
    :pid; (Passport ID)
    :cid; (Country ID)
    })

(defn valid-passport?
  [p]
  (empty? (set/difference (disj fields :cid) (set (keys p)))))

(->> input
     parse-passports
     (filter valid-passport?)
     count)
;;=> 202






(s/def ::byr (s/and #(re-matches #"\d{4}" %) #(<= 1920 (Integer/valueOf %) 2002)))
(s/def ::iyr (s/and #(re-matches #"\d{4}" %) #(<= 2010 (Integer/valueOf %) 2020)))
(s/def ::eyr (s/and #(re-matches #"\d{4}" %) #(<= 2020 (Integer/valueOf %) 2030)))
(s/def ::hgt (s/or ::in #(when-let [n (some-> (re-find #"(\d+)in" %) second Integer/valueOf)]
                           (<= 59 n 76))
                   ::cm #(when-let [n (some-> (re-find #"(\d+)cm" %) second Integer/valueOf)]
                           (<= 150 n 193))))
(s/def ::hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def ::ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(s/def ::pid #(re-matches #"[0-9]{9}" %))

(s/def ::passport (s/keys :req-un [::ecl ::byr ::iyr ::hgt ::pid ::hcl ::eyr]
                          :opt-un [::cid]))

(defn detailed-valid-passport?
  [p]
  (s/valid? ::passport p))

(->> input
     parse-passports
     (filter detailed-valid-passport?)
     count)
;;=> 137
