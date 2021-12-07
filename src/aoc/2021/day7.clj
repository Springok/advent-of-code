; [Day 7 - Advent of Code 2021](https://adventofcode.com/2021/day/7)
(ns aoc.2021.day7
  (:require
    [aoc.util :as util]))

(def example-positions
  (util/->integers "16,1,2,0,4,2,7,1,2,14"))

(def positions
  (-> (util/read-file "../resources/aoc/day7.txt")
      (util/->integers)))

(defn fuel-move [pos final-position]
   (Math/abs (- pos final-position)))

(defn fuel-consume-table [ra]
  (zipmap ra (map #(reduce + (range 1 (inc %))) ra)))

(def mfuel-consume-table (memoize fuel-consume-table))

(defn final-position [ps]
  (key (apply max-key val (frequencies ps))))

(defn average [ps]
  (/ (reduce + ps) (count ps)))

(comment
  (average positions)
  (average example-positions)
  (key (last (sort-by key (frequencies positions))))
  (final-position positions)

  ;; example
  (reduce (fn [ttl-f pos]
            (+ ttl-f (fuel-move pos (final-position example-positions))))
          0
          example-positions)

  (map #(reduce (fn [ttl-f pos]
                 (+ ttl-f (fuel-move pos %)))
           0
           example-positions) (range (apply min example-positions) (apply max example-positions)))

  ;; part1 => 326132
  (apply min (map #(reduce (fn [ttl-f pos]
                             (+ ttl-f (fuel-move pos %)))
                    0
                    positions) (range (apply max positions))))

  ;; part2 => 88612508
  (let [min-p (apply min positions)
        max-p (apply max positions)
        fuel-table (mfuel-consume-table (range min-p max-p))]
   (apply min (map #(reduce (fn [ttl-f pos]
                              (let [n (fuel-move pos %)]
                                (+ ttl-f (get fuel-table n 0))))
                     0
                     positions) (range min-p max-p)))))


;; binary search
;; https://github.com/callum-oakley/advent-of-code/blob/main/src/aoc/2021/07.clj
