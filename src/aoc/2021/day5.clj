; [Day 5 - Advent of Code 2021](https://adventofcode.com/2021/day/5)
(ns aoc.2021.day5
  (:require
    [aoc.util :as util]
    [clojure.core.matrix :as mat]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(defn line-to-cordination-pairs [s-line]
  (mat/array (->> (str/split s-line #" -> ")
                  (map util/->integers))))

(defn lines [path]
  (->> (util/read-file-by-line path)
       (map line-to-cordination-pairs)))

(defn effective-x-line? [[[_ y1] [_ y2]]] ;; [ [[x1 y1] [x2 y2]] [] ]
  (= y1 y2))

(defn effective-y-line? [[[x1 _] [x2 _]]] ;; [ [[x1 y1] [x2 y2]] [] ]
  (= x1 x2))

(defn effective-dia-line? [pair] ;; [ [[x1 y1] [x2 y2]] [] ]
  (not (or (effective-x-line? pair)
           (effective-y-line? pair))))

(defn generate-dia-points [[[x1 y1] [x2 y2]]]
  (let [x-range (if (> x1 x2) (range x1 (dec x2) -1) (range x1 (inc x2)))
        y-range (if (> y1 y2) (range y1 (dec y2) -1) (range y1 (inc y2)))]
    (map (fn [x y] [x y]) x-range y-range)))

(comment
  (range 6 2 -1))

(defn generate-points [[[x1 y1] [x2 y2]]]
  (cond
    (= x1 x2) (for [y (range y1 (+ 1 y2))]
                 [x1 y])
    (= y1 y2) (for [x (range x1 (+ 1 x2))]
                 [x y1])
    :else (generate-dia-points [[x1 y1] [x2 y2]])))

(defn part1 [input]
  (let [x-lines (map sort (filter effective-x-line? input))
        y-lines (map sort (filter effective-y-line? input))
        points  (concat (reduce concat [] (map generate-points x-lines))
                        (reduce concat [] (map generate-points y-lines)))]
       (->> (frequencies points)
            (filter #(-> % val (> 1)))
            (count))))

(defn part2 [input]
  (let [x-lines (map sort (filter effective-x-line? input))
        y-lines (map sort (filter effective-y-line? input))
        dia-lines (filter effective-dia-line? input)
        points  (concat (reduce concat [] (map generate-points x-lines))
                        (reduce concat [] (map generate-points y-lines))
                        (reduce concat [] (map generate-points dia-lines)))]

       (->> (frequencies points)
            (filter #(-> % val (> 1)))
            (count))))

(comment
  (part1 (lines "../resources/aoc/2021/day5.txt")) ;; => 7318
  (part2 (lines "../resources/aoc/2021/day5.txt"))) ;; =≥ 19939

(deftest test-example
  (let [sample (lines "../resources/aoc/2021/day5-ex.txt")]
    (is (= 5 (part1 sample)))
    (is (= 12 (part2 sample)))))
