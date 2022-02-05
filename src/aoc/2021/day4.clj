; [Day 4 - Advent of Code 2021](https://adventofcode.com/2021/day/4)
(ns aoc.2021.day4
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.test :refer [deftest is]]))

(defn game-boards [path]
  (->> (util/read-chunks path)
       (map #(str/replace % #"\n" " "))
       (map str/trim)
       (map #(str/split % #" +"))))

(def example
  (game-boards "../resources/aoc/2021/day4-ex.txt"))

(def input
  (game-boards "../resources/aoc/2021/day4.txt"))

(def example-order
  (map str [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]))

(def order
  (map str [83 69 34 46 30 23 19 75 22 37 89 78 32 39 11 44 95 43 26 48 84 53 94 88 18 40 62 35 27 42 15 2 91 20 4 64 99 71 54 97 52 36 28 7 74 45 70 86 98 1 61 50 68 6 77 8 57 47 51 72 65 3 49 24 79 13 17 92 41 80 63 67 82 90 55 0 10 93 38 21 59 73 33 31 9 76 5 66 16 58 85 87 12 29 25 14 96 56 60 81]))

(defn bingo-in-rows [rows n-per-row]
  (for [x (range 0 (* n-per-row rows) n-per-row)]
    (set (range x (+ x n-per-row)))))

(defn bingo-in-cols [cols n-per-row]
  (for [x (range 0 cols)]
    (set (range x (* cols n-per-row) n-per-row))))

(defn x-pos-list
  [board]
  (set (keep-indexed #(if (= "X" %2) %1 nil) board)))

(defn bingo?
  [board]
  (not-empty (filter #(= % (set/intersection % (x-pos-list board))) (concat (bingo-in-rows 5 5) (bingo-in-cols 5 5)))))

(defn mark-x [board number]
  (map #(if (= number %) "X" %) board))

(defn play-board
  [board order]
  (reduce (fn [r o]
            (if (or (bingo? (:b r)) (= o (last order)))
              r
              (-> r
                  (update :b #(mark-x % o))
                  (assoc :o (Integer/parseInt o))
                  (update :count inc))))
          {:o nil :count 0 :b board}
          order))

(defn part1 [input order]
  (let [boards (for [board input]
                 (play-board board order))
        bingo (apply min-key :count boards)]
    (* (:o bingo) (apply + (map #(Integer/parseInt %) (remove #(= "X" %) (:b bingo)))))))

(defn part2 [input order]
  (let [boards (for [board input]
                 (play-board board order))
        bingo (apply max-key :count boards)]
    (* (:o bingo) (apply + (map #(Integer/parseInt %) (remove #(= "X" %) (:b bingo)))))))

(comment
  (part1 input order) ;; => 41668
  (part2 input order)) ;; => 10478

(deftest test-example
  (let [sample example]
    (is (= 4512 (part1 sample example-order)))
    (is (= 1924 (part2 sample example-order)))))
