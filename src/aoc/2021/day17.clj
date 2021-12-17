; [Day 17 - Advent of Code 2021](https://adventofcode.com/2021/day/17)
(ns aoc.2021.day17
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
  "target area: x=20..30, y=-10..-5")

(def input
  "target area: x=117..164, y=-140..-89")

(defn parse [input]
  (->> (util/->integers input)
       (partition 2)
       (map #(range (first %) (inc (last %))))))

; ====================================================================

(defn x-v-change [xv]
  (if (> xv 0) (dec xv) 0))

(defn y-v-change [yv]
  (dec yv))

(defn on-target? [[xp yp] [x-t-range y-t-range]]
  (boolean (and ((set x-t-range) xp)
                ((set y-t-range) yp))))

(defn probe-positions [[x-init y-init] [x-t-range y-t-range] [init-xv init-yv]]
  (loop [idx 0
         xv init-xv
         yv init-yv
         xp x-init
         yp y-init
         h []]
    (if (or (on-target? [xp yp] [x-t-range y-t-range])
            (> xp (apply max x-t-range))
            (< yp (apply min y-t-range)))
       {:steps idx :init-v [init-xv init-yv] :max-h (apply max h) :on-target? (on-target? [xp yp] [x-t-range y-t-range])}
       (recur (inc idx) (x-v-change xv) (y-v-change yv) (+ xp xv) (+ yp yv) (conj h yp)))))

; ====================================================================
;; Initial guess about initial x velocity

(defn find-v [dis]
  (loop [idx 0]
    (if (<= dis (/ (* idx (+ idx 1)) 2))
      idx
      (recur (inc idx)))))

(defn estimate-xv-range [x-t-range]
  (let [min-x (apply min x-t-range)
        max-x (apply max x-t-range)]
    (range (find-v min-x) (inc max-x)))) ; 1 ~ n steps

(defn estimate-v
  ([x-t-range]
   (estimate-v x-t-range 1))
  ([x-t-range multiplier]
   (let [x-range (estimate-xv-range x-t-range)
         y-xrange (* multiplier (apply min x-range))
         y-range (range (- y-xrange) (inc y-xrange))]
    [x-range y-range])))

; ====================================================================

; trial and errors to find the boundary, hardcode multiplier: 10
(defn part1 [input]
  (let [[x-t-range y-t-range] (parse input)
        [x-v-range y-v-range] (estimate-v x-t-range 10)
         initial-point [0 0]]
     (->> (for [xv x-v-range
                yv y-v-range]
            (probe-positions initial-point [x-t-range y-t-range] [xv yv]))
          (filter :on-target?)
          (map :max-h)
          (apply max))))

(defn part2 [input]
  (let [[x-t-range y-t-range] (parse input)
        [x-v-range y-v-range] (estimate-v x-t-range 10)
         initial-point [0 0]]
     (->> (for [xv x-v-range
                yv y-v-range]
            (probe-positions initial-point [x-t-range y-t-range] [xv yv]))
          (filter :on-target?)
          (map :init-v)
          (set)
          (count))))

(comment
  ; 9730
  (part1 input)

  ; 4110
  (part2 input))

(deftest test-example
  (is (= 45 (part1 example)))
  (is (= 112 (part2 example))))

(comment
  (estimate-v (range 20 (inc 30)))
  (find-v 30)
  (probe-positions [0 0] [(range 20 (inc 30)) (range -10 (inc -5))] [7 9])
  [(range 20 (inc 30)) (range -10 (inc -5))]
  (apply min (range 20 (inc 30)))
  (estimate-v (first (parse input)))
  (part1 example))
