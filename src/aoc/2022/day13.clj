(ns aoc.2022.day13
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def example
  (util/read-chunks "../resources/aoc/2022/day13-ex.txt"))

(def input
  (util/read-chunks "../resources/aoc/2022/day13.txt"))

(defn parse-input [input]
  (->> input
       (map #(str/split % #"\n"))
       (flatten)
       (map read-string)))

(defn compare-ba [l r]
  (cond
    (every? number? [l r]) (compare l r)
    (number? l) (recur [l] r)
    (number? r) (recur l [r])
    :else (or (->> (map compare-ba l r)
                   (drop-while zero?)
                   (first))
              (- (count l) (count r)))))

(defn part1 [input]
  (->> (parse-input input)
       (partition 2)
       (keep-indexed (fn [idx [l r]] (when (neg? (compare-ba l r))
                                       (inc idx))))
       (apply +)))

(defn part2 [input]
  (->> (parse-input input)
       (concat [[[2]] [[6]]])
       (sort compare-ba)
       (keep-indexed (fn [idx packet] (when (#{[[2]] [[6]]} packet)
                                        (inc idx))))
       (apply *)))

(defn -main [& _]
  (println "part 1:" (time (part1 input)))
  (println "part 2:" (time (part2 input))))

(deftest test-example
  (is (= 13 (part1 example)))
  (is (= 140 (part2 example)))
  (is (= -1 (compare-ba 1 [2])))
  (is (= -1 (compare-ba [1 1 3] [1 1 5])))
  (is (=  1 (compare-ba 9 [8 7 6])))
  (is (= -2 (compare-ba [[4 4] 4 4] [[4 4] 4 4 4 4])))
  (is (= -1 (compare-ba [[4 4] 4 4] [[4 4] 4 4 4])))
  (is (= -1 (compare-ba [[1],[2,3,4]] [[1] 4])))
  (is (=  1 (compare-ba [9] [8 7 6])))
  (is (= -1 (compare-ba [4 4] [4 4 4])))
  (is (=  0 (compare-ba [4 4] [4 4])))
  (is (= -1 (compare-ba [] [3])))
  (is (=  1 (compare-ba [7 7 7 7] [7 7 7])))
  (is (=  1 (compare-ba [[[]]] [[]])))
  (is (=  1 (compare-ba [1,[2,[3,[4,[5,6,7]]]],8,9] [1,[2,[3,[4,[5,6,0]]]],8,9]))))

(comment
  (-main))
; (out) "Elapsed time: 3.437083 msecs"
; (out) part 1: 6076
; (out) "Elapsed time: 5.087667 msecs"
; (out) part 2: 24805
