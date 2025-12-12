(ns aoc2025.day-04
  (:require [aoc2025.read :as read]
            [clojure.string :as str]))

(defn input []
  (->> (read/read-day 4 "a")
       str/split-lines))

(defn get-surroundings [l row col]
  (map (fn [x] (->> x
                    (drop col)
                    (take 3)))
       (->> l
            (drop row)
            (take 3))))

(defn get-at [table row col notice-borders]
  (let [margin (if notice-borders 1 0)]
    (nth (nth table (+ row margin))
         (+ col margin))))

(defn get-ch [n]
  (if (>= n 4) \@ \x))

(defn get-square [l row col]
  (if (not= (get-at l row col true) \@) "."
      (let [surroundings (get-surroundings l row col)]
        (->> surroundings
             (apply concat)
             (filter (partial = \@))
             count dec
             get-ch))))

(defn add-borders [l]
  (let [bc "#"
        br (list (repeat (count l) bc))]
    (concat br
            (map (fn [x] (concat bc x bc)) l)
            br)))

(defn mark-accessible [l]
  (let [board (add-borders l)]
    (map (fn [x] (map (fn [y] (get-square board x y))
                      (range (count l))))
         (range (count l)))))

(defn count-forklifts [marked-map]
  (->> marked-map
       (apply concat)
       (filter (partial = \x))
       count))

(defn count-removeable [l]
  (let [marked-map (mark-accessible l)
        forklifts count-forklifts]
    (println "forklifts in this move: " forklifts)
    (if (= forklifts 0) 0
        (+ forklifts (count-removeable marked-map)))))

(defn start-a []
  (->> (input)
       mark-accessible
       (apply concat)
       (filter (partial = \x))
       count))

(defn start-b []
  (->> (input)
       count-removeable))

(defn start [part]
  (cond (= part :a) (start-a)
        (= part :b) (start-b)))
