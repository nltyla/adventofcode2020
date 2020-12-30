(ns adventofcode2020.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn day1
  "--- Day 1: Report Repair ---"
  [n name]
  (let [v (inputs name #(Integer/parseInt %))
        in (take n (repeat v))
        perm0 (map vector v)
        perms (reduce (fn [acc vl] (mapcat (fn [v] (map #(conj % v) acc)) vl)) perm0 in)
        x (comp (map #(conj % (reduce + %)))
                (filter #(= 2020 (last %)))
                (map #(apply * (butlast %)))
                (distinct))]
    (first (into [] x perms))))

(def day1-1 (partial day1 1))

(def day1-2 (partial day1 2))

(defn char-count
  "returns the number of occurrences of char needle in string s"
  [needle s]
  (transduce (map #(if (= needle %) 1 0)) + s))

(def pwd-pred (fn [low high letter pwd] (<= low (char-count letter pwd) high)))

(defn letter-value [pwd letter index]
  (if (= letter (nth pwd (dec index))) 1 0))

(def pwd-pred-2
  (fn [low high letter pwd]
    (let [f (partial letter-value pwd letter)]
      (= 1 (+ (f low) (f high))))))

(defn pwd-value
  "return 0 for invalid, 1 for valid rule"
  [pred rule]
  (if-let [[_ low high [letter] pwd] (re-find #"(\d+)-(\d+) (.): (.*)" rule)]
    (let [low (Integer/parseInt low)
          high (Integer/parseInt high)]
      (if (pred low high letter pwd) 1 0))
    0))

(defn day2
  "--- Day 2: Password Philosophy ---"
  [pred name]
  (let [s (inputs name identity)
        f (partial pwd-value pred)]
    (transduce (map f) + s)))

(def day2-1 (partial day2 pwd-pred))

(def day2-2 (partial day2 pwd-pred-2))

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

(defn day4
  "--- Day 4: Passport Processing ---"
  [preds name]
  (let [s (inputs name identity)
        xf (comp (partition-by #(str/blank? %))
                 (map (partial str/join " "))
                 (filter (complement empty?))
                 (map #(str/split % #"\s|:"))
                 (map #(apply hash-map %))
                 (map #(validate-passport preds %)))]
    (transduce xf + s)))

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

(def true-pred (constantly true))

(def day4-1 (partial day4 {"byr" true-pred,
                           "iyr" true-pred,
                           "eyr" true-pred,
                           "hgt" true-pred,
                           "hcl" true-pred,
                           "ecl" true-pred,
                           "pid" true-pred}))

(def day4-2 (partial day4 {"byr" byr-pred,
                           "iyr" iyr-pred,
                           "eyr" eyr-pred,
                           "hgt" hgt-pred,
                           "hcl" hcl-pred,
                           "ecl" ecl-pred,
                           "pid" pid-pred}))