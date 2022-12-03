(ns aoc.2022.day3
  (:require
    [aoc.util :as util]
    [clojure.set :as cset]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line  "../resources/aoc/2022/day3-ex.txt")
       (map char-array)))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day3.txt")
       (map char-array)))

; https://www.asciitable.com/, a-z 97 ~ 122, A~Z 65 ~ 90
(def priority-map
  (zipmap (map char (concat (range 97 (inc 122)) (range 65 (inc 90))))
          (range 1 (inc (* 26 2)))))

(defn solve-priority [items]
  (->> items
       (map set)
       (apply cset/intersection)
       (map #(priority-map %))))

(defn part1 [input]
   (->> input
        (map #(partition (/ (count %) 2) %))
        (mapcat solve-priority)
        (apply +)))

(defn part2 [input]
  (->> input
       (partition 3)
       (mapcat solve-priority)
       (apply +)))

(deftest test-example
  (is (= 157 (part1 example)))
  (is (= 70 (part2 example))))

(comment
  (time (part1 input))  ; 8109 => "Elapsed time: 2.458625 msecs"
  (time (part2 input))) ; 2738 => "Elapsed time: 1.292417 msecs"
