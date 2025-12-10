(ns aoc2025.day-01
  (:require [aoc2025.read :as read]
            [clojure.string :as clstr]))

;; PART A
(def initial-status {:rotation 50 :count 0})

(defn into-instructions [lines]
  (map (fn [x] {:turn (if (= \L (first x)) :l :r)
                :distance (read-string (apply str (rest x)))})
       lines))

(defn rotate [previous-position instruction]
  (+ previous-position
     (* (:distance instruction)
        (if (= (:turn instruction) :l) -1 1))))

(defn apply-instruction [status instruction]
  (let [new-rotation (rotate (:rotation status) instruction)]
    {:rotation new-rotation
     :count (+ (:count status)
               (if (= (mod new-rotation 100) 0) 1 0))}))

(defn crack-password [instructions]
  (-> (reduce apply-instruction
              initial-status
              instructions) :count))

(defn start-a []
  (let [input (read/read-day 1 "a")
        lines (clstr/split-lines input)]
    (crack-password (into-instructions lines))))

(defn start-b []
  (let [input (read/read-day 1 "a")
        lines (clstr/split-lines input)]
    (crack-password (into-instructions lines))))

(defn start [part]
  (cond (= part :a) (start-a)
        (= part :b) (start-b)))
