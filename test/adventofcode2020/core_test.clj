(ns adventofcode2020.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2020.core :refer :all]))

(deftest day1-test
  (is (= 514579 (day1 "1-example.txt")))
  (is (= 1019371 (day1 "1.txt"))))

(deftest day2-test
  (is (= 2 (day2 "2-example.txt")))
  (is (= 418 (day2 "2.txt"))))

(deftest day3-test
  (is (= 7 (day3 "3-example.txt")))
  (is (= 276 (day3 "3.txt"))))

(deftest day4-test
  (is (= 2 (day4 "4-example.txt")))
  (is (= 182 (day4 "4.txt"))))
