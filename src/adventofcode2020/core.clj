(ns adventofcode2020.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn day1
  "Day 1: Report Repair"
  []
  (let [v (inputs "1.txt" #(Integer/parseInt %))
        vv (map vector v)
        x (comp (mapcat (fn [e] (map #(conj % e) vv)))
                (map #(conj % (+ (% 0) (% 1))))
                (filter #(= 2020 (% 2)))
                (map #(* (% 0) (% 1)))
                (distinct))]
    (into [] x v)))

;-----

(defn char-count
  "returns the number of occurrences of char needle in string s"
  [needle s]
  (transduce (map #(if (= needle %) 1 0)) + s))

(defn valid-pwd
  "return 0 for invalid, 1 for valid rule"
  [rule]
  (let [[low high letter pwd] (rest (re-find #"(\d+)-(\d+) (.): (.*)" rule))
        lowi (Integer/parseInt low)
        highi (Integer/parseInt high)
        cnt (char-count (first letter) pwd)]
    (if (<= lowi cnt highi) 1 0)))

(defn day2
  "Day 2: Password Philosophy"
  []
  (let [s (inputs "2.txt" identity)]
    (transduce (map valid-pwd) + s)))

;-----
