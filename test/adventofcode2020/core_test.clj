(ns adventofcode2020.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2020.core :refer :all]))

(deftest day1-test
  (is (= 514579 (day1 "1-example.txt")))
  (is (= 1019371 (day1 "1.txt"))))

(deftest day1-part2-test
  (is (= 241861950 (day1-part2 "1-example.txt")))
  (is (= 278064990 (day1-part2 "1.txt"))))

(deftest day2-test
  (is (= 2 (day2 "2-example.txt")))
  (is (= 418 (day2 "2.txt"))))

(deftest day3-test
  (is (= 7 (day3 "3-example.txt")))
  (is (= 276 (day3 "3.txt"))))

(deftest day4-test
  (is (= 2 (day4 "4-example.txt")))
  (is (= 182 (day4 "4.txt"))))

(deftest day4-part2-test
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
  (is (= 0 (day4-part2 "4-2-example-invalid.txt")))
  (is (= 4 (day4-part2 "4-2-example-valid.txt")))
  (is (= 109 (day4-part2 "4.txt")))
  )