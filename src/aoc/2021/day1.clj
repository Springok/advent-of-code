; [Day 1 - Advent of Code 2021](https://adventofcode.com/2021/day/1)
(ns aoc.2021.day1
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day1.txt")
       (map #(Integer/parseInt %))))

(def example
  [199 200 208 210 200 207 240 269 260 263])

(defn part1 [records]
  (->> (partition 2 1 records)
       (filter #(apply < %))
       (count)))

(defn part2 [records]
  (->> (partition 3 1 records)
       (map #(reduce + %))
       (part1)))

(deftest test-example
   (let [sample example]
     (is (= 7 (part1 sample)))
     (is (= 5 (part2 sample)))))
