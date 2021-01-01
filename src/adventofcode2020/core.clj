(ns adventofcode2020.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn inputs
  "read file from resources, apply f to each line, return seq"
  [name f]
  (map f (str/split-lines (slurp (io/resource name)))))

(defn day1-1-core
  [v]
  (for [x v y v :when (= 2020 (+ x y))] [x y]))

(defn day1-2-core
  [v]
  (let [z1 (for [x v y v :let [t (+ x y)] :when (<= t 2020)] [x y t])]
    (for [x v [y1 y2 y3] z1 :when (= (+ x y3) 2020)] [x y1 y2])))

(defn day1
  "--- Day 1: Report Repair ---"
  [f name]
  (let [v (inputs name #(Integer/parseInt %))]
    (reduce * (first (f v)))))

(def day1-1 (partial day1 day1-1-core))
(def day1-2 (partial day1 day1-2-core))

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
  [rowstep colstep name]
  (let [grid (take-nth rowstep (inputs name cycle))
        offsets (iterate #(+ colstep %) 0)
        trees (map (fn [row offset] (if (= \# (nth row offset)) 1 0)) grid offsets)]
    (reduce + trees)))

(def day3-1 (partial day3 1 3))

(defn day3-2
  [name]
  (* (day3 1 1 name) (day3 1 3 name) (day3 1 5 name) (day3 1 7 name) (day3 2 1 name)))

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

(defn binstr-to-dec
  [s]
  (loop [sr (reverse s)
         d 0
         m 1]
    (if (empty? sr)
      d
      (let [c (first sr)
            d' (if (or (= \R c) (= \B c)) (+ d m) d)]
        (recur (rest sr) d' (* 2 m))))))

(defn day5
  "--- Day 5: Binary Boarding ---"
  [name]
  (let [s (inputs name identity)]
    (apply max (map binstr-to-dec s))))

(defn day5-2
  [name]
  (let [s (inputs name identity)
        p (sort (map binstr-to-dec s))]
    (loop [prev (first p)
           r (rest p)]
      (let [prev' (inc prev)]
        (if (not= (first r) prev')
          prev'
          (recur prev' (rest r)))))))

(defn day6
  "--- Day 6: Custom Customs ---"
  [f name]
  (let [s (inputs name identity)
        xf (comp (partition-by #(str/blank? %))
                 (filter #(not= [""] %))
                 (map #(map (fn [member] (set member)) %))
                 (map #(apply f %))
                 (map #(count %)))]
    (transduce xf + s)))

(def day6-1 (partial day6 set/union))

(def day6-2 (partial day6 set/intersection))

(defn keywordize [names] (keyword (str/join "-" names)))

(defn parse-bag-line
  [s]
  (let [[_ & rest] (re-find #"(\w+) (\w+) bags" s)
        key (keywordize rest)
        m (re-matcher #"(\d+) (\w+) (\w+) bag" s)]
    (loop [[match count & rest] (re-find m)
           result {}]
      (if-not match
        [key result]
        (recur (re-find m) (assoc result (keywordize rest) (Integer/parseInt count)))))
    ))

(def children (fn [m key] (keys (m key))))

(def children-expanded (fn [m key] (reduce-kv (fn [acc k v] (apply conj acc (repeat v k))) [] (m key))))

(defn bag-reachable
  [f target root]
  (let [bags (tree-seq true-pred f root)]
    (if (some #(= target %) bags)
      1
      0))
  )

(defn day7-1
  "--- Day 7: Handy Haversacks ---"
  [name]
  (let [s (inputs name parse-bag-line)
        m (into {} s)
        target :shiny-gold
        f (partial bag-reachable (partial children m) target)
        k (filter #(not= target %) (keys m))]
    (transduce (map f) + k)))

(defn day7-2
  "--- Day 7: Handy Haversacks ---"
  [name]
  (let [s (inputs name parse-bag-line)
        m (into {} s)
        bags (tree-seq true-pred (partial children-expanded m) :shiny-gold)]
    (count (rest bags))
    ))