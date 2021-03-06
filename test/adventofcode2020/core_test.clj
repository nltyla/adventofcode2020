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

(deftest day13-2-test
  (is (= 3417 (day13-2 "13-2-example-1.txt")))
  (is (= 754018 (day13-2 "13-2-example-2.txt")))
  (is (= 779210 (day13-2 "13-2-example-3.txt")))
  (is (= 1261476 (day13-2 "13-2-example-4.txt")))
  (is (= 1202161486 (day13-2 "13-2-example-5.txt"))))

(deftest day14-1-test
  (let [mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"]
    (is (= 73 (apply-mask 11 mask)))
    (is (= 101 (apply-mask 101 mask)))
    (is (= 64 (apply-mask 0 mask)))
    (is (= 5875750429995 (day14-1 "14.txt")))))

(deftest day14-2-test
  (is (= '(26 27 58 59) (gen-addrs 42 "000000000000000000000000000000X1001X")))
  (is (= 5272149590143 (day14-2 "14.txt"))))

(deftest day15-1-test
  (is (= 436 (day15 [0 3 6] 2020)))
  (is (= 1 (day15 [1 3 2] 2020)))
  (is (= 10 (day15 [2 1 3] 2020)))
  (is (= 27 (day15 [1 2 3] 2020)))
  (is (= 78 (day15 [2 3 1] 2020)))
  (is (= 438 (day15 [3 2 1] 2020)))
  (is (= 1836 (day15 [3 1 2] 2020)))
  (is (= 240 (day15 [14 8 16 0 1 17] 2020))))

; slow, 30 secs
(deftest day15-2-test
  (is (= 505 (day15 [14 8 16 0 1 17] 30000000))))

(deftest day16-1-test
  (is (= 29759 (day16-1 "16.txt"))))

(deftest day16-2-test
  (is (= 1307550234719 (day16-2 "16.txt"))))

(deftest day17-1-test
  (is (= 112 (day17 "17-example.txt" 2)))
  (is (= 237 (day17 "17.txt" 2))))

(deftest day17-2-test
  (is (= 848 (day17 "17-example.txt" 3)))
  (is (= 2448 (day17 "17.txt" 3))))

(deftest day18-1-test
  (is (= nil (eval18-1 "")))
  (is (= 26 (eval18-1 "2 * 3 + (4 * 5)")))
  (is (= 437 (eval18-1 "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  (is (= 12240 (eval18-1 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
  (is (= 13632 (eval18-1 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))
  (is (= 3348222486398 (day18-1 "18.txt"))))

(deftest day18-2-test
  (is (= nil (eval18-2 "")))
  (is (= 46 (eval18-2 "2 * 3 + (4 * 5)")))
  (is (= 1445 (eval18-2 "5 + (8 * 3 + 9 + 3 * 4 * 3)")))
  (is (= 669060 (eval18-2 "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")))
  (is (= 23340 (eval18-2 "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")))
  (is (= 43423343619505 (day18-2 "18.txt"))))

(deftest day19-1-test
  (is (= 2 (day19-1 "19-example.txt")))
  (is (= 107 (day19-1 "19.txt"))))
