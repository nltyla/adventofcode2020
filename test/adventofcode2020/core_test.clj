(ns adventofcode2020.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2020.core :refer :all]))

(deftest day1-1-test
  (is (= 514579 (day1-1 "1-example.txt")))
  (is (= 1019371 (day1-1 "1.txt"))))

(deftest day1-2-test
  (is (= 241861950 (day1-2 "1-example.txt")))
  (is (= 278064990 (day1-2 "1.txt"))))

(deftest day2-1-test
  (is (= 2 (day2-1 "2-example.txt")))
  (is (= 418 (day2-1 "2.txt"))))

(deftest day2-2-test
  (is (= 1 (day2-2 "2-example.txt")))
  (is (= 616 (day2-2 "2.txt"))))

(deftest day3-1-test
  (is (= 7 (day3-1 "3-example.txt")))
  (is (= 276 (day3-1 "3.txt"))))

(deftest day3-2-test
  (is (= 336 (day3-2 "3-example.txt")))
  (is (= 7812180000 (day3-2 "3.txt"))))

(deftest day4-1-test
  (is (= 2 (day4-1 "4-example.txt")))
  (is (= 182 (day4-1 "4.txt"))))

(deftest day4-2-test
  (is (true? (byr-pred "2002")))
  (is (false? (byr-pred "2003")))
  (is (true? (hgt-pred "60in")))
  (is (true? (hgt-pred "190cm")))
  (is (false? (hgt-pred "190in")))
  (is (false? (hgt-pred "190")))
  (is (true? (hcl-pred "#123abc")))
  (is (false? (hcl-pred "#123abz")))
  (is (false? (hcl-pred "123abc")))
  (is (true? (ecl-pred "brn")))
  (is (false? (ecl-pred "wat")))
  (is (true? (pid-pred "000000001")))
  (is (false? (pid-pred "0123456789")))
  (is (= 0 (day4-2 "4-2-example-invalid.txt")))
  (is (= 4 (day4-2 "4-2-example-valid.txt")))
  (is (= 109 (day4-2 "4.txt")))
  )

(deftest day5-1-test
  (is (= 357 (binstr-to-dec "FBFBBFFRLR")))
  (is (= 820 (day5 "5-example.txt")))
  (is (= 951 (day5 "5.txt"))))

(deftest day5-2-test
  (is (= 653 (day5-2 "5.txt"))))

(deftest day6-1-test
  (is (= 11 (day6-1 "6-example.txt")))
  (is (= 6504 (day6-1 "6.txt"))))

(deftest day6-2-test
  (is (= 6 (day6-2 "6-example.txt")))
  (is (= 3351 (day6-2 "6.txt"))))

(deftest day7-1-test
  (is (= 4 (day7-1 "7-example.txt")))
  (is (= 161 (day7-1 "7.txt"))))

(deftest day7-2-test
  (is (= 126 (day7-2 "7-2-example.txt")))
  (is (= 30899 (day7-2 "7.txt"))))

(deftest day8-1-test
  (is (= 5 (day8-1 "8-example.txt")))
  (is (= 1548 (day8-1 "8.txt"))))

(deftest day8-2-test
  (is (= 8 (day8-2 "8-example.txt")))
  (is (= 1375 (day8-2 "8.txt"))))

(deftest day9-1-test
  (is (= 127 (day9-1 "9-example.txt" 5)))
  (is (= 50047984 (day9-1 "9.txt" 25))))

(deftest day9-2-test
  (is (= 62 (day9-2 "9-example.txt" 127)))
  (is (= 5407707 (day9-2 "9.txt" 50047984))))

(deftest day10-1-test
  (is (= 35 (day10-1 "10-example-1.txt")))
  (is (= 220 (day10-1 "10-example-2.txt")))
  (is (= 2210 (day10-1 "10.txt"))))

(deftest day10-2-test
  (is (= 8 (day10-2 "10-example-1.txt")))
  (is (= 19208 (day10-2 "10-example-2.txt")))
  (is (= 7086739046912 (day10-2 "10.txt"))))

(deftest day11-1-test
  (is (= 37 (day11-1 "11-example.txt")))
  (is (= 2183 (day11-1 "11.txt"))))

(def parse-occupieds (partial parse-area #(= \# %)))
(defn visible-occupied-seats-tester
  [name seat]
  (let [s (inputs name identity)
        maxrow (dec (count s))
        maxcol (dec (count (first s)))
        seats (parse-seats s)
        occupieds (parse-occupieds s)]
    (visible-occupied-seats maxrow maxcol seats occupieds seat)))

(deftest day11-2-test
  (is (= 8 (visible-occupied-seats-tester "11-2-example-1.txt" [4 3])))
  (is (= 0 (visible-occupied-seats-tester "11-2-example-2.txt" [1 1])))
  (is (= 0 (visible-occupied-seats-tester "11-2-example-3.txt" [3 3])))
  (is (= 26 (day11-2 "11-example.txt")))
  (is (= 1990 (day11-2 "11.txt"))))

(deftest day12-1-test
  (is (= 25 (day12-1 "12-example.txt")))
  (is (= 420 (day12-1 "12.txt"))))

(deftest day12-2-test
  (is (= 286 (day12-2 "12-example.txt")))
  (is (= 42073 (day12-2 "12.txt"))))

(deftest day13-1-test
  (is (= 295 (day13-1 "13-example.txt")))
  (is (= 104 (day13-1 "13.txt"))))
