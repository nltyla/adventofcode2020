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
    (+ (Math/abs ^int (:lat out)) (Math/abs ^int (:lon out)))))

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
    (+ (Math/abs ^int (:lat out)) (Math/abs ^int (:lon out)))))

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
  (let [sv (reverse (sort-by first v))
        [step start] (first sv)]
    (loop [n (- start)]
      (let [n' (+ n step)]
        (if (every? (fn [p] (= (mod (+ n (second p)) (first p)) 0)) sv)
          n
          (recur n'))))))

; while correct, the clojure code runs too slowly to solve the problem
; solved by handcoding some java that completes on 1 core in about 30 mins
;
;public static long solve() {
;    long myN = -17L;
;    long myStep = 907L;
;    while (true) {
;        myN += myStep;
;        if ((myN + 17L) % 907L == 0
;            && (myN + 48L) % 653L == 0
;            && (myN + 58L) % 41L == 0
;            && (myN + 11L) % 37L == 0
;            && (myN + 46L) % 29L == 0
;            && (myN + 40L) % 23L == 0
;            && (myN + 29L) % 19L == 0
;            && myN % 17L == 0
;            && (myN + 61L) % 13L == 0) {
;                return myN;
;        }
;    }
;}
(defn day13-2
  "--- Day 13, Part Two: Shuttle Search ---"
  [name]
  (let [s (inputs name identity)
        ids (str/split (second s) #",")
        numoffs (reduce-kv (fn [acc k v]
                             (if (= v "x")
                               acc
                               (conj acc (list (Integer/parseInt v) k))))
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

(defn speak
  [m spoken turn]
  (lazy-seq
    (let [turn' (inc turn)
          turns (m spoken)
          spoken' (if (= 1 (count turns))
                    0
                    (apply - turns))
          m' (update m spoken' #(take 2 (conj % turn')))]
      (cons spoken' (speak m' spoken' turn')))))

(defn day15
  "--- Day 15: Rambunctious Recitation ---"
  [in n]
  (let [m (reduce-kv (fn [acc k v] (update acc v #(conj % (inc k)))) {} in)
        n (- n (inc (count in)))]
    (nth (speak m (peek in) (count in)) n)))

(defn parse-pred
  [s]
  (let [[_ l1 h1 l2 h2] (re-matches #".*: (\d+)-(\d+) or (\d+)-(\d+)" s)]
    (fn [x] (or (<= (Integer/parseInt l1) x (Integer/parseInt h1))
                (<= (Integer/parseInt l2) x (Integer/parseInt h2))))))

(defn day16-1
  "--- Day 16: Ticket Translation ---"
  [name]
  (let [s (inputs name identity)
        preds (map parse-pred (take 20 s))
        somefn (apply some-fn preds)
        xf (comp
             (drop 25)
             (mapcat #(str/split % #","))
             (map #(Integer/parseInt %))
             (filter #(not (somefn %))))]
    (transduce xf + s)))

(defn strs-to-ints
  [v]
  (mapv #(Integer/parseInt %) (str/split v #",")))

(defn day16-2
  "--- Day 16, Part Two: Ticket Translation ---"
  [name]
  (let [s (inputs name identity)
        preds (map parse-pred (take 20 s))                  ; fields are in first 20 rows
        somefn (apply some-fn preds)
        my-ticket (strs-to-ints (nth s 22))                 ; my ticket is in row 23
        tickets' (->> s
                      (drop 25)                             ; tickets start at row 26
                      (map strs-to-ints)
                      (filter (fn [row] (every? #(somefn %) row)))
                      (apply mapv vector))                  ; transpose
        result (->> (for [index-valrow (map-indexed vector tickets')
                          index-pred (map-indexed vector preds)
                          :when (every? (second index-pred) (second index-valrow))]
                      [(first index-valrow) (first index-pred)])
                    (partition-by first)
                    (map (fn [p] (reduce (fn [acc v] [(v 0) (conj (set (peek acc)) (v 1))]) [] p)))
                    (cons [-1 #{}])
                    (sort-by #(count (peek %)))
                    (partition 2 1)
                    (map (fn [[[_ s1] [row s2]]] [row (first (set/difference s2 s1))]))
                    (filter #(<= 0 (peek %) 5))             ; first 6 rows are departure fields
                    (map first)
                    (map #(nth my-ticket %))
                    (apply *))]
    result))

(defn parse-slice-n
  [pred strs n]
  (set (for [idx-row (map-indexed vector strs)
             col (keep-indexed #(if (pred %2) %1) (idx-row 1))]
         (into [(idx-row 0) col 0] (repeat (- n 2) 0)))))

(def parse-cells-n (partial parse-slice-n #(not= \. %)))

(defn offsets-n [n]
  (let [ranges (repeat n [0 1 -1])]
    (rest (reduce (fn [indices-so-far new-range]
                    (mapcat (fn [el] (map #(conj % el) indices-so-far)) new-range))
                  (map vector (first ranges)) (rest ranges)))))

(defn offset-n [cell offset] (vec (map + cell offset)))
(defn neighbors-n [cell] (set (map #(offset-n cell %) (offsets-n (count cell)))))
(def mem-neighbors-n (memoize neighbors-n))
(defn live-neighbors-n [cells cell] (set/intersection (mem-neighbors-n cell) cells))
(defn void-neighbors-n [cells cell] (set/difference (mem-neighbors-n cell) cells))
(defn count-live-neighbors-n [cells cell] (count (live-neighbors-n cells cell)))
(defn cell-survives-n? [cells cell] (let [n (count-live-neighbors-n cells cell)] (or (= 2 n) (= 3 n))))
(defn survivors-n [cells] (set (filter #(cell-survives-n? cells %) cells)))
(defn cell-born-n? [cells cell] (= 3 (count-live-neighbors-n cells cell)))
(defn potential-births-n [cells] (set (apply set/union (map #(void-neighbors-n cells %) cells))))
(defn births-n [cells] (set (filter #(cell-born-n? cells %) (potential-births-n cells))))
(defn next-gen-n [cells] (set/union (survivors-n cells) (births-n cells)))
(defn life-n [cells] (iterate next-gen-n cells))

(defn day17
  "--- Day 17: Conway Cubes ---"
  [name n]
  (let [s (inputs name identity)
        cells (parse-cells-n s n)
        gens (life-n cells)]
    (count (nth gens 6))))

(defn eval-err
  [s expected]
  ((throw (Exception. (str "Syntax error. rest: \"" (str/join s) "\" expected: \"" expected "\"")))))

(defn consume-char
  [s c]
  (if (= (first s) c)
    (rest s)
    (eval-err s c)))

(defn consume-int
  [s]
  [(Integer/parseInt (str (first s))) (rest s)])

(def eval-left)
(def eval-op)
(def eval-parens)
(defn digit? [c] (<= (int \0) (int c) (int \9)))

(defn eval-right
  [s]
  (let [tok (first s)]
    (cond (digit? tok) (consume-int s)
          (= \( tok) (eval-parens s)
          :else (eval-err s "digit | ("))))

(defn eval-operator
  [operator [left s]]
  (let [[right s1] (eval-right (rest s))
        left1 (operator left right)]
    (eval-op [left1 s1])))

(defn eval-operator-l
  [operator [left s]]
  (let [[right s1] (eval-left (rest s))
        left1 (operator left right)]
    [left1 s1]))

(def plus-op (partial eval-operator +))
(def ^:dynamic *mul-op* (partial eval-operator *))

(defn eval-op
  [[_ s :as all]]
  (if (empty? s)
    all
    (let [tok (first s)
          f (cond (= \+ tok) plus-op
                  (= \* tok) *mul-op*
                  (= \)) identity
                  :else (eval-err s "+ | * | )"))]
      (f all))))

(defn eval-parens
  [s]
  (let [s1 (consume-char s \()
        [left1 s2] (eval-left s1)
        s3 (consume-char s2 \))]
    [left1 s3]))

(defn eval-left
  [s]
  (when-let [tok (first s)]
    (cond (digit? tok) (->> s
                            (consume-int)
                            (eval-op))
          (= \( tok) (->> s
                          (eval-parens)
                          (eval-op))
          :else
          (eval-err s "digit | ("))))

(defn eval18-1
  [s]
  (first (eval-left (str/replace s " " ""))))

(defn day18-1
  [name]
  (let [s (inputs name identity)]
    (transduce (map eval18-1) + s)))

(defn eval18-2
  [s]
  (binding [*mul-op* *mul-op*]
    (set! *mul-op* (partial eval-operator-l *))
    (first (eval-left (str/replace s " " "")))))

(defn day18-2
  [name]
  (let [s (inputs name identity)]
    (transduce (map eval18-2) + s)))

(defn substitute
  [m k]
  (if (string? k)
    k
    (let [v (m k)]
      (if (string? v)
        v
        (transduce (map #(substitute m %)) str v)))))

(defn parse-tuple
  [s]
  (let [tuple (mapcat #(list "(" (Integer/parseInt %) ")") (str/split s #" "))]
    (concat ["("] tuple [")"])))

(defn parse-rule
  [s]
  (let [[key vals] (str/split s #": ")
        k (Integer/parseInt key)
        v (if (str/starts-with? vals "\"")
            (subs vals 1 2)
            (->> (str/split vals #" \| ")
                 (map parse-tuple)
                 (interpose ["|"])
                 (apply concat)))]
    [k v]))

(defn parse-rules
  [coll]
  (transduce (map parse-rule) conj {} coll))

(defn day19-1
  [name]
  (let [s (inputs name identity)
        m (parse-rules (take-while not-empty s))
        regex (substitute m 0)
        pattern (re-pattern (str regex "$"))
        messages (rest (drop-while not-empty s))
        matches (filter some? (map #(re-matches pattern %) messages))]
    (count matches)))