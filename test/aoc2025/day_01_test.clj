(ns aoc2025.day-01-test
  (:require [clojure.test :refer :all]
            [aoc2025.day-01 :refer :all]))

(defn mymap []
  (map (fn [x] (* x 2)) [1 2 3 4]))

(deftest a-test
  (testing "Mapping a function"
    (is (= [2 4 6 8] (mymap)))))

(deftest string-to-number
  (testing "Transforming string to number"
    (is (= 89 (read-string (apply str (rest "a89")))))))

(deftest modtest
  (testing "Negative modules"
    (is (= 4 (mod -96 100)))))
