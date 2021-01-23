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
        m (re-matcher #"(\d+) (\w+) (\w+) bag" s)
        matches (take-while some? (repeatedly #(re-find m)))
        result (reduce (fn [acc [_ count & rest]] (assoc acc (keywordize rest) (Integer/parseInt count))) {} matches)]
    [key result]))

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
  "--- Day 7 Part Two: Handy Haversacks ---"
  [name]
  (let [s (inputs name parse-bag-line)
        m (into {} s)
        bags (tree-seq true-pred (partial children-expanded m) :shiny-gold)]
    (count (rest bags))
    ))

(defn acc-instruction
  [n state]
  (-> state
      (update :acc #(+ n %))
      (update :pc inc)))

(defn jmp-instruction
  [n state]
  (update state :pc #(+ n %)))

(defn nop-instruction
  [state]
  (update state :pc inc))

(defn compile-instruction
  [s]
  (let [[_ ins n] (re-matches #"(acc|jmp|nop) ([+-]\d+)" s)
        ni (Integer/parseInt n)]
    (cond (= ins "acc") (partial acc-instruction ni)
          (= ins "jmp") (partial jmp-instruction ni)
          (= ins "nop") nop-instruction)))

(defn day8-1
  "--- Day 8: Handheld Halting ---"
  [name]
  (let [instructions (into [] (inputs name compile-instruction))]
    (loop [state {:pc 0 :acc 0}
           seen #{}]
      (let [pc (:pc state)
            seen? (contains? seen pc)
            instruction (instructions pc)]
        (if seen?
          (:acc state)
          (recur (instruction state) (conj seen pc)))))))

(defn parse-instruction
  [s]
  (let [[_ ins n] (re-matches #"(acc|jmp|nop) ([+-]\d+)" s)
        ni (Integer/parseInt n)]
    (cond (= ins "acc") [:acc ni]
          (= ins "jmp") [:jmp ni]
          (= ins "nop") [:nop ni])))


(defn acc-instruction-2
  [operand state]
  (-> state
      (update :acc #(+ operand %))
      (update :pc inc)))

(declare nop-instruction-2)

(defn jmp-instruction-2
  [operand state flip]
  (if flip
    (nop-instruction-2 operand state (not flip))
    (update state :pc #(+ operand %))))

(defn nop-instruction-2
  [operand state flip]
  (if flip
    (jmp-instruction-2 operand state (not flip))
    (update state :pc inc)))

(defn interpret
  [instructions iter]
  (loop [state {:pc 0 :acc 0}
         seen #{}]
    (let [pc (:pc state)
          seen? (contains? seen pc)]
      (cond seen? [:fail (:acc state)]
            (= pc (count instructions)) [:success (:acc state)]
            :else (let [[opcode operand flip-iter] (instructions pc)
                        flip (= iter flip-iter)
                        state' (cond (= opcode :acc) (acc-instruction-2 operand state)
                                     (= opcode :jmp) (jmp-instruction-2 operand state flip)
                                     (= opcode :nop) (nop-instruction-2 operand state flip))]
                    (recur state' (conj seen pc))))))
  )

(defn day8-2
  "--- Day 8 Part Two: Handheld Halting ---"
  [name]
  (let [instructions (into [] (inputs name parse-instruction))
        pc-with-nop-jmp (keep-indexed #(if (or (= :nop (%2 0)) (= :jmp (%2 0))) %1) instructions)
        pc-seqno (map-indexed #(list %2 %1) pc-with-nop-jmp)
        mod-instructions (reduce (fn [acc idxs] (update acc (first idxs) #(conj % (second idxs)))) instructions pc-seqno)
        max-iter (count pc-with-nop-jmp)]
    (loop [iter 0]
      (let [[outcome acc] (interpret mod-instructions iter)]
        (cond (= outcome :success) acc
              (> iter max-iter) (println "fail iter " iter " acc" acc)
              :else (recur (inc iter)))))))

(defn init-9
  [coll]
  (loop [c1 coll
         sums {}]
    (if-let [c2 (next c1)]
      (let [term (first c1)
            r' (reduce #(update %1 (+ term %2) (fnil inc 0)) sums c2)]
        (recur c2 r'))
      sums)))

(defn delta-sums
  [n v f]
  (reduce #(update %1 (+ n %2) (fnil f 0)) {} v))

(defn day9-1
  "--- Day 9: Encoding Error ---"
  [name n]
  (let [s (into [] (inputs name #(Long/parseLong %)))]
    (loop [sums (init-9 (take n s))
           in s]
      (let [c (in n)]
        (if (> (get sums c 0) 0)
          (let [evicted-sums (delta-sums (in 0) (subvec in 1 n) dec)
                admitted-sums (delta-sums c (subvec in 1 n) inc)
                sums' (merge-with + sums evicted-sums admitted-sums)]
            (recur sums' (subvec in 1)))
          c))
      )
    )
  )

(def sum (memoize
           (fn
             [from to v]
             (if (= from to)
               (v from)
               (+ (v to) (sum from (dec to) v))))))

(defn day9-2
  "--- Day 9: Encoding Error ---"
  [name n]
  (let [s (into [] (inputs name #(Long/parseLong %)))
        ranges (for [x (range (count s)) y (range (count s)) :when (> y x)] [x y])
        sums (take-while #(not= n (sum (% 0) (% 1) s)) ranges)
        matching-range (nth ranges (count sums))
        matching-vector (subvec s (matching-range 0) (matching-range 1))]
    (+ (apply max matching-vector) (apply min matching-vector))))

(defn day10-1
  "--- Day 10: Adapter Array ---"
  [name]
  (let [s (inputs name #(Integer/parseInt %))
        device-jolt (+ 3 (apply max s))
        out (conj (apply sorted-set s) 0 device-jolt)
        jolt-freqs (->> out
                        (partition 2 1)
                        (map #(- (second %) (first %)))
                        frequencies)]
    (* (jolt-freqs 1) (jolt-freqs 3))))

(defn candidates
  [acc jolt']
  (let [in (first (acc (dec (count acc))))
        in' (disj in jolt')]
    (conj acc [in' jolt'])))

(def adapter-search
  (memoize
    (fn
      [in jolt]
      (if (empty? in)
        1
        (let [max-jolt (+ 3 jolt)
              attempts (rest (reduce candidates [[in jolt]] (subseq in > jolt <= max-jolt)))]
          (reduce + (map #(adapter-search (% 0) (% 1)) attempts)))))))

(defn day10-2
  "--- Day 10 Part Two: Adapter Array ---"
  [name]
  (let [s (inputs name #(Integer/parseInt %))
        device-jolt (+ 3 (apply max s))
        out (adapter-search (apply sorted-set (conj s device-jolt)) 0)]
    out))

(defn offset [cell offset] (vec (map + cell offset)))
(defn neighbors [cell] (set (map #(offset cell %) [[-1 1] [0 1] [1 1] [-1 0] [1 0] [-1 -1] [0 -1] [1 -1]])))
(defn live-neighbors [cells cell] (set/intersection (neighbors cell) cells))
(defn count-live-neighbors [cells cell] (count (live-neighbors cells cell)))

(defn parse-area
  [pred strs]
  (set (for [idx-row (map-indexed vector strs)
             col (keep-indexed #(if (pred %2) %1) (idx-row 1))]
         [(idx-row 0) col])))

(def parse-seats (partial parse-area #(not= \. %)))

(defn ngen [seats occupieds]
  (reduce (fn
            [occupieds' seat]
            (let [nb (count-live-neighbors occupieds seat)]
              (cond (contains? occupieds seat) (if (>= nb 4)
                                                 (disj occupieds' seat)
                                                 occupieds')
                    :else (if (= nb 0)
                            (conj occupieds' seat)
                            occupieds'))
              )) occupieds seats))

(defn day11-1
  "--- Day 11 : Seating System ---"
  [name]
  (let [s (inputs name identity)
        seats (parse-seats s)]
    (loop [occupieds #{}]
      (let [occupieds' (ngen seats occupieds)]
        (if (= occupieds occupieds')
          (count occupieds)
          (recur occupieds'))))))

(defn ray [seat offset] (iterate #(vec (map + % offset)) seat))
(defn rays [seat] (map #(rest (ray seat %)) [[-1 1] [0 1] [1 1] [-1 0] [1 0] [-1 -1] [0 -1] [1 -1]]))
(defn in-area? [maxrow maxcol [row col]] (and (<= 0 row maxrow) (<= 0 col maxcol)))

(defn visible-occupied-seat [maxrow maxcol seats occupieds ray]
  (let [bounded-ray (drop-while #(and (in-area? maxrow maxcol %) (not (contains? seats %))) ray)
        seat-candidate (first bounded-ray)]
    (if (contains? occupieds seat-candidate) 1 0)))

(defn visible-occupied-seats [maxrow maxcol seats occupieds seat]
  (let [seat-rays (rays seat)
        f (partial visible-occupied-seat maxrow maxcol seats occupieds)]
    (transduce (map f) + seat-rays)))

(defn ngen2 [maxrow maxcol seats occupieds]
  (reduce (fn
            [occupieds' seat]
            (let [nb (visible-occupied-seats maxrow maxcol seats occupieds seat)]
              (cond (contains? occupieds seat) (if (>= nb 5)
                                                 (disj occupieds' seat)
                                                 occupieds')
                    :else (if (= nb 0)
                            (conj occupieds' seat)
                            occupieds'))
              )) occupieds seats))

(defn day11-2
  "--- Day 11, Part Two : Seating System ---"
  [name]
  (let [s (inputs name identity)
        maxrow (dec (count s))
        maxcol (dec (count (first s)))
        seats (parse-seats s)]
    (loop [occupieds #{}]
      (let [occupieds' (ngen2 maxrow maxcol seats occupieds)]
        (if (= occupieds occupieds')
          (count occupieds)
          (recur occupieds'))))))

(defn parse-12
  [s]
  (let [[_ actionstr valstr] (re-matches #"([NSEWLRF])(\d+)" s)
        action (keyword actionstr)
        value (Integer/parseInt valstr)]
    [action value]))

(defn apply-instruction
  [acc [action value]]
  (cond (= action :N) (update acc :lat #(+ % value))
        (= action :S) (update acc :lat #(- % value))
        (= action :E) (update acc :lon #(+ % value))
        (= action :W) (update acc :lon #(- % value))
        (= action :L) (update acc :hdg #(mod (- % value) 360))
        (= action :R) (update acc :hdg #(mod (+ % value) 360))
        (= action :F) (let [hdg (:hdg acc)]
                        (cond (= hdg 0) (apply-instruction acc [:N value])
                              (= hdg 90) (apply-instruction acc [:E value])
                              (= hdg 180) (apply-instruction acc [:S value])
                              (= hdg 270) (apply-instruction acc [:W value])))))

(defn day12-1
  "--- Day 12: Rain Risk ---"
  [name]
  (let [s (inputs name parse-12)
        out (reduce apply-instruction {:lat 0 :lon 0 :hdg 90} s)]
    (+ (Math/abs (:lat out)) (Math/abs (:lon out)))))

(defn apply-instruction-2
  [acc [action value]]
  (cond (= action :N) (update acc :waylat #(+ % value))
        (= action :S) (update acc :waylat #(- % value))
        (= action :E) (update acc :waylon #(+ % value))
        (= action :W) (update acc :waylon #(- % value))
        (= action :L) (let [lat (:waylat acc)
                            lon (:waylon acc)]
                        (cond (= value 270) (-> acc
                                                (assoc :waylat (- lon))
                                                (assoc :waylon lat))
                              (= value 180) (-> acc
                                                (assoc :waylat (- lat))
                                                (assoc :waylon (- lon)))
                              (= value 90) (-> acc
                                               (assoc :waylat lon)
                                               (assoc :waylon (- lat)))))
        (= action :R) (apply-instruction-2 acc [:L (- 360 value)])
        (= action :F) (-> acc
                          (update :lat #(+ % (* value (:waylat acc))))
                          (update :lon #(+ % (* value (:waylon acc)))))))

(defn day12-2
  "--- Day 12, Part Two: Rain Risk ---"
  [name]
  (let [s (inputs name parse-12)
        out (reduce apply-instruction-2 {:lat 0 :lon 0 :waylat 1 :waylon 10} s)]
    (+ (Math/abs (:lat out)) (Math/abs (:lon out)))))

(defn parse-13
  [s]
  (map #(Integer/parseInt %) (filter #(not= % "x") (str/split s #","))))

(defn day13-1
  "--- Day 13: Shuttle Search ---"
  [name]
  (let [s (inputs name parse-13)
        target (first (first s))
        ids (second s)
        nearests (map #(- (* % (inc (quot target %))) target) ids)
        [idx min] (apply min-key second (map-indexed vector nearests))]
    (println ids nearests idx min)
    (* min (nth ids idx))))

(defn solver13 [v]
  (let [sv (vec (reverse (sort-by first v)))
        [step start] (first sv)]
    (loop [n (- start)]
      (let [n' (+ n step)]
        (if (every? (fn [p] (= (mod (+ n (p 1)) (p 0)) 0)) sv)
          n
          (recur n'))))))

(defn day13-2
  "--- Day 13, Part Two: Shuttle Search ---"
  [name]
  (let [s (inputs name identity)
        ids (str/split (second s) #",")
        numoffs (reduce-kv (fn [acc k v]
                             (if (= v "x")
                               acc
                               (conj acc [(Integer/parseInt v) k])))
                           []
                           ids)
        ans (solver13 numoffs)]
    ans))

(defn apply-mask
  [^long n mask]
  (reduce-kv (fn [acc k v] (cond (= \0 v) (bit-clear acc k)
                                 (= \1 v) (bit-set acc k)
                                 :else acc)) n (vec (reverse mask))))

(defn process-line-14
  [acc s]
  (if (str/starts-with? s "mask")
    (assoc acc :mask (subs s 7))
    (let [[_ addrstr decstr] (re-matches #"mem\[(\d+)\] = (\d+)" s)
          n (Long/parseLong decstr)
          n' (apply-mask n (:mask acc))]
      (assoc-in acc [:mem addrstr] n'))))

(defn day14-1
  "--- Day 14: Docking Data ---"
  [name]
  (let [s (inputs name identity)
        run (reduce process-line-14 {:mask "" :mem {}} s)]
    (reduce + (vals (:mem run)))))

(defn gen-addrs
  [^long n mask]
  (reduce-kv (fn [acc k v] (cond (= \X v) (concat (map #(bit-clear % k) acc) (map #(bit-set % k) acc))
                                 (= \1 v) (map #(bit-set % k) acc)
                                 :else acc)) (list n) (vec (reverse mask))))

(defn process-line-14-2
  [acc s]
  (if (str/starts-with? s "mask")
    (assoc acc :mask (subs s 7))
    (let [[_ addrstr decstr] (re-matches #"mem\[(\d+)\] = (\d+)" s)
          addr (Long/parseLong addrstr)
          n (Long/parseLong decstr)
          addrs (gen-addrs addr (:mask acc))]
      (reduce (fn [acc' addr] (assoc-in acc' [:mem addr] n)) acc addrs))))

(defn day14-2
  "--- Day 14, Part Two: Docking Data ---"
  [name]
  (let [s (inputs name identity)
        run (reduce process-line-14-2 {:mask "" :mem {}} s)]
    (reduce + (vals (:mem run)))))