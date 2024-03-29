(ns aoc.2022.day4
  (:require
    [aoc.util :as util]
    [clojure.set :as cset]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line  "../resources/aoc/2022/day4-ex.txt")))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day4.txt")))

(defn parse-sections [assignment]
  (->> (re-seq #"\d+" assignment)
       (map read-string)
       (partition 2)
       (map (fn [[start end]]
              (set (range start (inc end)))))))

(defn fully? [[a1 a2]]
  (or (cset/subset? a1 a2)
      (cset/subset? a2 a1)))

(defn partially? [[a1 a2]]
  (seq (cset/intersection a1 a2)))

(defn solve [input filter-fn]
  (->> input
       (map parse-sections)
       (filterv filter-fn)
       (count)))

(defn part1 [input]
  (solve input fully?))

(defn part2 [input]
  (solve input partially?))

(deftest test-example
  (is (= 2 (part1 example)))
  (is (= 4 (part2 example))))

(comment
  (time (part1 input))  ; 524 => "Elapsed time: 11.72 msecs"
  (time (part2 input))) ; 798 => "Elapsed time: 9.543291 msecs"
