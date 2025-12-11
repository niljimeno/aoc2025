(ns aoc2025.day-03
  (:require [aoc2025.read :as read]
            [clojure.string :as clstr]))

(defn into-digits [word]
  (->> word
       (map int)
       (map #(- % (int \0)))))

(defn keyed-max [xs]
  (reduce (fn [l x] (if (> (last x) (last l)) x l))
          [0 0]
          (map-indexed vector xs)))

(defn largest-joltage [batteries]
  (let [local-maximum (keyed-max (drop-last batteries))]
    (+ (* (last local-maximum)
          10)
       (->> (drop (-> (first local-maximum)
                      (+ 1))
                  batteries)
            (apply max)))))

(defn recursive-joltage [times batteries]
  (if (= times 0) []
      (let [lm (apply max (drop-last (dec times) batteries))
            li (.indexOf batteries lm)]
        (conj (recursive-joltage (dec times) (drop (inc li) batteries)) lm))))

(defn digits-to-int [l]
  (reduce (fn [a b] (+ b (* a 10))) 0 l))

(defn start-a []
  (let [input (read/read-day 3 "a")]
    (->> input
         clstr/split-lines
         (map into-digits)
         (map largest-joltage)
         (reduce +))))

(defn start-b []
  (let [input (read/read-day 3 "a")]
    (->> input
         clstr/split-lines
         (map into-digits)
         (map (partial recursive-joltage 12))
         (map reverse)
         (map (partial digits-to-int))
         (reduce +))))

(defn start [part]
  (cond (= part :a) (start-a)
        (= part :b) (start-b)))
