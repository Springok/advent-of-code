; [Day 13 - Advent of Code 2021](https://adventofcode.com/2021/day/13)
(ns aoc.2021.day13
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(defn strs->integers [strs]
   (map #(Integer/parseInt %) strs))

(def example
  (->> (util/read-chunks "../resources/aoc/2021/day13-ex.txt")))

(def input
  (->> (util/read-chunks "../resources/aoc/2021/day13.txt")))

(defn folding-lines [inputs]
  (->> (last inputs)
       (str/split-lines)
       (map #(re-seq #"x|y|\d+" %))))

(defn marked-points [inputs]
  (->> (first inputs)
       (str/split-lines)
       (map #(str/split % #","))
       (map strs->integers)
       (set)))


(defn fold-up [folding-line points]
  (let [y-line (Integer/parseInt (second folding-line))
        folding-points (->> points
                            (filter #(> (second %) y-line))
                            (map (fn [[x y]]
                                   [x (- y-line (- y y-line))])))]
    (->> points
         (filter #(< (second %) y-line))
         (apply conj folding-points)
         (set))))

(defn fold-left [folding-line points]
  (let [x-line (Integer/parseInt (second folding-line))
        folding-points (->> points
                            (filter #(> (first %) x-line))
                            (map (fn [[x y]]
                                  [(- x-line (- x x-line)) y])))]
    (->> points
         (filter #(< (first %) x-line))
         (apply conj folding-points)
         (set))))

(defn fold [folding-line points]
  (if (= "y" (first folding-line))
     (fold-up folding-line points)
     (fold-left folding-line points)))

(defn draw2 [dots]
  (->> (for [y (range (inc (apply max (map second dots))))]
         (for [x (range (inc (apply max (map first dots))))]
           (if (dots [x y]) "#" " ")))
       (map str/join)
       (str/join "\n")
       (str/split-lines)))

(defn part1 [inputs]
  (let [folding-line (first (folding-lines inputs))]
    (->> (marked-points inputs)
         (fold folding-line)
         (count))))


(defn part2 [inputs]
  (let [folding-lines (folding-lines inputs)
        points (marked-points inputs)]
    (->> (reduce (fn [ac line]
                  (fold line ac))
            points
            folding-lines)
         (draw2))))

(comment
  ; first attempt: 1017 (fail)
  ; second attempt: 837 (pass)
  (part1 input)

  ; => EPZGKCHU
  (part2 input))

(deftest test-example
  (is (= 17 (part1 example))))

(comment
  (apply conj #{[1 2]} [`(1 2)])
  (if ((set (marked-points example)) [0 3]) "X" "-")
  (Integer/parseInt "23")
  (folding-lines example)
  (folding-lines input)
  (marked-points input)
  (re-seq #"x|y|\d" "fold along y=7"))
