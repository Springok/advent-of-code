(ns aoc.2022.day2
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
  [[\A \Y] [\B \X] [\C \Z]])

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day2.txt")
       (map char-array)
       (map (fn [[a _space b]] [a b]))))

(def scores
  {\X {\A (+ 1 3) \B (+ 1 0) \C (+ 1 6)}
   \Y {\A (+ 2 6) \B (+ 2 3) \C (+ 2 0)}
   \Z {\A (+ 3 0) \B (+ 3 6) \C (+ 3 3)}})

(def part2-rule
  {\X {\A \Z \B \X \C \Y}
   \Y {\A \X \B \Y \C \Z}
   \Z {\A \Y \B \Z \C \X}})

(defn part1 [input]
  (->> input
       (map (fn [[op you]]
              (get-in scores [you op])))
       (reduce +)))


(defn part2 [input]
  (->> input
       (map (fn [[op you]]
              (let [you'(get-in part2-rule [you op])]
                (get-in scores [you' op]))))
       (reduce +)))

(deftest test-example
  (is (= 15 (part1 example)))
  (is (= 12 (part2 example))))

(comment
  (time (part1 input))  ; 13924 => "Elapsed time: 0.57975 msecs"
  (time (part2 input))) ; 13448 => "Elapsed time: 1.001792 msecs"
