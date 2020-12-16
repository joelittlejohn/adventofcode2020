(ns adventofcode2020.d2
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "2.txt"))]
    (->> (slurp r) str/split-lines)))

;; regex version
(defn regex-parse
  [s]
  (let [[_ a b c p] (re-find #"(\d+)-(\d+) ([a-z]): ([a-z]+)" s)]
    [(Integer/valueOf a) (Integer/valueOf b) (first c) p]))

(defn validate
  [[a b c p]]
  (let [n (count (filter #{c} p))]
    (<= a n b)))

(->> input (map regex-parse) (filter validate) count)
;; => 424









;; spec version
(s/def ::letter
  (->> (range 97 123) (map char) set))

(s/def ::number
  (s/* (->> (range 48 58) (map char) set)))

(s/def ::range
  (s/cat :number-a ::number
         :hyphen #{\-}
         :number-b ::number))

(s/def ::password
  (s/* ::letter))

(s/def ::policy
  (s/cat :range ::range :space #{\space} :letter ::letter))

(s/def ::password-line
  (s/cat :policy ::policy
         :colon #{\:}
         :space #{\space}
         :password ::password))

(defn spec-parse
  [s]
  (let [{{{a :number-a b :number-b} :range l :letter} :policy p :password} (s/conform ::password-line s)]
    [(Integer/valueOf (apply str a))
     (Integer/valueOf (apply str b))
     l
     (apply str p)]))

(->> input (map seq) (map spec-parse) (filter validate) count)
;; => 424







(defn official-toboggan-validate
  [[a b c p]]
  (not= (= c (.charAt p (dec a))) (= c (.charAt p (dec b)))))

(->> input (map seq) (map spec-parse) (filter official-toboggan-validate) count)
;; => 747

(->> input (map regex-parse) (filter official-toboggan-validate) count)
;; => 747
