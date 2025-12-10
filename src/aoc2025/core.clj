(ns aoc2025.core
  (:require [aoc2025.day-01 :as day-01])
  (:gen-class))

(defn -main
  [& _]
  (println (day-01/start :a)
           (day-01/start :b)))
