(ns aoc.2022.day0
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
  (util/read-file-by-line "../resources/aoc/2022/day0-ex.txt"))

(def input
  (util/read-file-by-line "../resources/aoc/2022/day0.txt"))

(defn part1 [input])

(defn part2 [input])

(deftest test-example
  (is (= 31 (part1 example)))
  (is (= 29 (part2 example))))

(defn -main [& _]
  (println "part 1:" (time (part1 input)))
  (println "part 2:" (time (part2 input))))

(comment
  (-main))
