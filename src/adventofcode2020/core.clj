(ns adventofcode2020.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn day1
  "--- Day 1: Report Repair ---"
  [name]
  (let [v (inputs name #(Integer/parseInt %))
        vv (map vector v)
        x (comp (mapcat (fn [e] (map #(conj % e) vv)))
                (map #(conj % (+ (% 0) (% 1))))
                (filter #(= 2020 (% 2)))
                (map #(* (% 0) (% 1)))
                (distinct))]
    (first (into [] x v))))

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
  "--- Day 2: Password Philosophy ---"
  [name]
  (let [s (inputs name identity)]
    (transduce (map valid-pwd) + s)))

(defn day3
  "--- Day 3: Toboggan Trajectory ---"
  [name]
  (let [grid (inputs name cycle)
        offsets (iterate #(+ 3 %) 0)
        trees (map (fn [row offset] (if (= \# (nth row offset)) 1 0)) grid offsets)]
    (reduce + trees)))

(defn day4
  "--- Day 4: Passport Processing ---"
  [name]
  (let [s (inputs name identity)
        req ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]
        xf (comp (partition-by #(str/blank? %))
                 (map (partial str/join " "))
                 (filter (complement empty?))
                 (map #(str/split % #"\s|:"))
                 (map #(apply hash-map %))
                 (map #(if (every? % req) 1 0)))]
    (transduce xf + s)))