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
