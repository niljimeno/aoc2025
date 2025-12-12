(ns aoc2025.day-05
  (:require [clojure.string :as str]))

(defn input []
  (->> (slurp "input/day5")
       (#(str/split % #"\n\n"))
       (map str/split-lines)
       (map (partial remove empty?))))

(defn format-range [r]
  (->> r
       (#(str/split % #"-"))
       (map read-string)))

(defn is-in-range [id r]
  (and (>= id (first r))
       (<= id (last r))))

(defn is-fresh [id ranges]
  (->> ranges
       (some (partial is-in-range id))))

(defn start-a []
  (let [input (input)
        ids (->> (last input)
                 (map read-string))
        ranges (->> (first input)
                    (map format-range))]
    (->> ids
         (filter #(is-fresh % ranges))
         count)))

(defn sort-first [op li]
  (sort (fn [l n] (op (first n)
                      (first l)))
        li))

(defn merge-helper [l n]
  (let [last-range (last l)]
    (if (or (empty? l)
            (> (first n) (last last-range)))
      (conj l n)
      (conj (vec (drop-last l))
            [(min (first last-range) (first n))
             (max (last last-range) (last n))]))))

(defn merge-ranges [previous]
  (let [new-ranges (reduce merge-helper [] previous)]
    (if (= new-ranges previous)
      new-ranges
      (merge-ranges new-ranges))))

(defn count-range [r]
  (+ (-> r first (* -1))
     (-> r last inc)))

(defn start-b []
  (let [input (input)
        ranges (->> (first input)
                    (map format-range))]
    (->> ranges
         (map vec)
         (sort-first >)
         merge-ranges
         (map count-range)
         (reduce +))))

(defn start [part]
  (cond (= part :a) (start-a)
        (= part :b) (start-b)))
