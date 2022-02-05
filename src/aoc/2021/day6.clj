; [Day 6 - Advent of Code 2021](https://adventofcode.com/2021/day/6)
(ns aoc.2021.day6
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
  [3 4 3 1 2])

(def input
  (-> (util/read-file "../resources/aoc/2021/day6.txt")
      (util/->integers)))

(defn initial-map [initial-state]
  (merge (zipmap (range 0 9) (repeat 9 0))
         (frequencies initial-state)))

(defn daily-update [current-map]
  (reduce (fn [d-map num]
            (case num
             8 (assoc d-map 8 (get current-map 0))
             6 (assoc d-map 6 (+ (get current-map 7) (get current-map 0)))
             (assoc d-map num (get current-map (inc num)))))
          {}
          (range 0 9)))

(defn gen [input dayto]
  (let [initial-map (initial-map input)]
    (reduce + (vals (:fish-map (reduce (fn [acc n]
                                         (-> acc
                                             (assoc :day n)
                                             (update :fish-map daily-update)))
                                       {:day 0 :fish-map initial-map}
                                       (range 1 (inc dayto))))))))

(defn part1 [input]
  (gen input 80))

(defn part2 [input]
  (gen input 256))

(comment
  (part1 input) ;; => 372300
  (part2 input)) ;; => 1675781200288

(deftest test-example
  (let [sample example]
    (is (= 5934 (part1 sample)))
    (is (= 26984457539 (part2 sample)))))
