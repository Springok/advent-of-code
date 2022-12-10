(ns aoc.2022.day10
  (:require
    [aoc.util :as util]
    [clojure.string :as s]
    [clojure.test :refer [deftest is]]))

(def simple-example
  ["noop"
   "addx 3"
   "addx -5"])

(def example
   (util/read-file-by-line "../resources/aoc/2022/day10-ex.txt"))

(def input
   (util/read-file-by-line "../resources/aoc/2022/day10.txt"))

(defn parse-commands [input]
  (->> input
       (map #(s/split % #" "))
       (map (fn [[command value]] (if (some? value)
                                    [command (read-string value)]
                                    [command])))))

(defn process [cycles input]
  (loop [nth-cycle 0
         commands (parse-commands input)
         result-value 1
         coming-values [0]
         crt-row []]
    (if (= nth-cycle cycles)
       {:result-value result-value :crt-row crt-row}
       (let [shift (* 40 (int (Math/floor (/ nth-cycle 40))))
             result-value (+ result-value (first coming-values))
             sprite-position (+ shift result-value)
             sprite-positions #{(dec sprite-position) sprite-position (inc sprite-position)}
             [command command-value] (first commands)
             values (case command
                      "addx" [0 command-value]
                      "noop" [0]
                      [0])]
         (recur (inc nth-cycle)
                (drop 1 commands)
                result-value
                (concat (drop 1 coming-values) values)
                (conj crt-row (if (sprite-positions (count crt-row)) "#" ".")))))))

(defn part1 [input]
  (let [cycles [20 60 100 140 180 220]]
    (->> cycles
        (map #(process % input))
        (map :result-value)
        (zipmap cycles)
        (reduce-kv (fn [value k v] (+ value (* k v))) 0))))

(defn part2 [input]
  (->> (:crt-row (process 240 input))
       (partition 40)
       (map s/join)))

(deftest test-example
  (is (= 4 (:result-value (process 5 simple-example))))
  (is (= 13140 (part1 example))))

(comment
  (time (part1 input))  ; 14560,    "Elapsed time: 1.685292 msecs"
  (time (part2 input))) ; EKRHEPUZ, "Elapsed time: 1.037208 msecs"

