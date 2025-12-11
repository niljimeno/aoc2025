(ns aoc2025.day-02
  (:require [aoc2025.read :as read]
            [clojure.string :as clstr]))

(defn is-palyndrome [s]
  (let [len (quot (count s) 2)]
    (= (take len s) (drop len s))))

(defn filter-valid [values]
  (filter
   (fn [x] (is-palyndrome (str x)))
   values))

(defn get-ranges [x]
  (->> (clstr/split x #"-")
       (map (fn [y] (-> (apply str y)
                        (read-string))))
       ((fn [x] (range (first x)
                       (+ (second x) 1))))))

(defn start-a []
  (let [input (read/read-day 2 "a")]
    (->> (clstr/split input #",")
         (map get-ranges)
         (map filter-valid)
         (apply concat)
         (reduce +))))

(defn is-sequence
  ([xs]
   (some (fn [y] (is-sequence (take y xs)
                              (drop y xs)))
         (range 1 (-> (count xs)
                      (quot 2)
                      (+ 1)))))
  ([x xs]
   (and (= x (take (count x) xs))
        (or (= x xs)
            (is-sequence x (drop (count x) xs))))))

(defn filter-valid-b [values]
  (filter
   (fn [x] (is-sequence (str x)))
   values))

(defn start-b []
  (let [input (read/read-day 2 "a")]
    (->> (clstr/split input #",")
         (map get-ranges)
         (map filter-valid-b)
         (apply concat)
         (reduce +))))

(defn start [part]
  (cond (= part :a) (start-a)
        (= part :b) (start-b)))
