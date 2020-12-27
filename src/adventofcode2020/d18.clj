(ns adventofcode2020.d18
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def input
  (with-open [r (io/reader (io/resource "18.txt"))]
    (->> (slurp r) str/split-lines)))
