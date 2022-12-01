; [Day 18 - Advent of Code 2021](https://adventofcode.com/2021/day/18)
(ns aoc.2021.day18
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]
    [clojure.edn :as edn]
    [clojure.zip :as zip]))

; binary tree...
; or zipper in clojure

; data structure: [x, y], [[x,y] y'] = [x', y'] => [x, y] = x'
; snailfish numbers must always be reduced
; condition for reduction:
;  If any pair is nested inside four pairs, the leftmost such pair explodes. e.g. 5th nested pair explodes
;    - explodes the element becomes zero
;  If any regular number is 10 or greater, the leftmost such regular number splits.
;    - the left element of the pair should be the regular number divided by two and rounded down

;  explode comes before split
; function for adding, split, explodes
; function calculate magnitude

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day18.txt")
       (map edn/read-string)))

(defn add-fish-numbers [a b]
  (->> [a b]
       (iterate reduce-fish-numbers)))

(defn find-explode [loc])

(defn explode [loc])

(defn split [loc]
  (let [n (/ (zip/node loc) 2)]
    (-> loc
        (zip/replace [(int (Math/floor n))
                      (int (Math/ceil n))])
        (zip/root))))

(defn reduce-fish-numbers [fish-number]
   fish-number)

(defn add-fish-numbers [a b]
  (->> [a b]
       (iterate reduce-fish-numbers)
       (reduce #(if (= %1 %2) (reduced %1) %2))))


(defn magnitdue [fish-numbers])

; ====================================================================

(defn part1 [input])

(defn part2 [input])

(comment
  (part1 input)

  (part2 input))

(def example
  [[[[0 [5 8]] [[1 7] [9 6]]] [[4 [1 2]] [[1 4] 2]]]])
   ; [[[5 [2 8]] 4] [5 [[9 9] 0]]]
   ; [6 [[[6 2] [5 6]] [[7 6] [4 7]]]]
   ; [[[6 [0 7]] [0 9]] [4 [9 [9 0]]]]
   ; [[[7 [6 4]] [3 [1 3]]] [[[5 5] 1] 9]]
   ; [[6 [[7 3] [3 2]]] [[[3 8] [5 7]] 4]]
   ; [[[[5 4] [7 7]] 8] [[8 3] 8]]
   ; [[9 3] [[9 9] [6 [4 9]]]]
   ; [[2 [[7 7] 7]] [[5 8] [[9 3] [0 2]]]]
   ; [[[[5 2] 5] [8 [3 7]]] [[5 [7 5]] [4 4]]]])

(comment
  (-> [[1 2] [3 [4 5] 6]]
      zip/vector-zip)

  (reduce add-fish-numbers example))

(deftest test-example
    (is (= 4140 (part1 example))))
    ; (is (= 112 (part2 homework)))))
