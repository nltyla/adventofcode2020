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
  (let [[_ low high letter pwd] (re-find #"(\d+)-(\d+) (.): (.*)" rule)
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

(defn validate-passport
  [preds p]
  (if (every? (fn [[key pred]] (and (contains? p key) (pred (p key)))) (seq preds)) 1 0))

(defn day4-shared
  [name preds]
  (let [s (inputs name identity)
        xf (comp (partition-by #(str/blank? %))
                 (map (partial str/join " "))
                 (filter (complement empty?))
                 (map #(str/split % #"\s|:"))
                 (map #(apply hash-map %))
                 (map #(validate-passport preds %)))]
    (transduce xf + s)))

(defn day4
  "--- Day 4: Passport Processing ---"
  [name]
  (let [true-pred (constantly true)]
    (day4-shared name {"byr" true-pred,
                       "iyr" true-pred,
                       "eyr" true-pred,
                       "hgt" true-pred,
                       "hcl" true-pred,
                       "ecl" true-pred,
                       "pid" true-pred})))

(defn yr-pred
  [from to]
  (fn [s] (and (some? (re-matches #"\d\d\d\d" s)) (<= from (Integer/parseInt s) to))))

(def byr-pred (yr-pred 1920 2002))
(def iyr-pred (yr-pred 2010 2020))
(def eyr-pred (yr-pred 2020 2030))

(def hgt-pred
  (fn [s] (if-let [cm (re-matches #"(\d\d\d)cm" s)]
            (<= 150 (Integer/parseInt (cm 1)) 193)
            (if-let [in (re-matches #"(\d\d)in" s)]
              (<= 59 (Integer/parseInt (in 1)) 76)
              false))))

(def hcl-pred (fn [s] (some? (re-matches #"#[0-9a-f]{6}" s))))

(def ecl-pred (fn [s] (contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} s)))

(def pid-pred (fn [s] (some? (re-matches #"\d{9}" s))))

(defn day4-part2
  "--- Day 4 Part Two : Passport Processing ---"
  [name]
  (day4-shared name {"byr" byr-pred,
                     "iyr" iyr-pred,
                     "eyr" eyr-pred,
                     "hgt" hgt-pred,
                     "hcl" hcl-pred,
                     "ecl" ecl-pred,
                     "pid" pid-pred}))
