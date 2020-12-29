(ns adventofcode2020.core-test
  (:require [clojure.test :refer :all]
            [adventofcode2020.core :refer :all]))

(deftest day1-example-test
  (testing "day 1 example"
    (is (= 514579 (day1 "1-example.txt")))))

(deftest day1-test
  (testing "day 1"
    (is (= 1019371 (day1 "1.txt")))))

(deftest day2-example-test
  (testing "day 2 example"
    (is (= 2 (day2 "2-example.txt")))))

(deftest day2-test
  (testing "day 2"
    (is (= 418 (day2 "2.txt")))))

(deftest day3-example-test
  (testing "day 3 example"
    (is (= 7 (day3 "3-example.txt")))))

(deftest day3-test
  (testing "day 3"
    (is (= 276 (day3 "3.txt")))))
