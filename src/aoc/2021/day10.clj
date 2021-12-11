; [Day 10 - Advent of Code 2021](https://adventofcode.com/2021/day/10)
(ns aoc.2021.day10
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2021/day10-ex.txt")))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day10.txt")))

(def valid-pairs
  #"\<\>|\[\]|\{\}|\(\)")

(def corruptted-pairs
  ;; #"\[\>")
  #"\(\>|\(\}|\(\]|\[\>|\[\)|\[\}|\{\]|\{\>|\{\)|\<\)|\<\]|\<\}")

(defn delete-pair [s]
  (str/replace s valid-pairs ""))

(defn examine [line]
  (loop [line line
         idx 0]
   (let [corruptted-pair (re-find corruptted-pairs line)
         corruptted? (not (str/blank? corruptted-pair))
         all-open? (set/superset? #{"(" "[" "{" "<"} (set (str/split line #"")))]
     ;; (prn line)
     ;; (prn all-open?)
     ;; (prn idx)
     ;; (prn corruptted-pairs)
     ;; (prn corruptted-pair)
     ;; (prn (not (str/blank? corruptted-pair)))
     (cond
       (= 300 idx) [line all-open?]
       corruptted? {:status "corruptted" :line line :info corruptted-pair}
       all-open?   {:status "incomplete" :line line}
       :else (recur (delete-pair line) (inc idx))))))

(defn closing-score [s]
  (->> (reverse (str/split s #""))
       (map {"(" ")" "[" "]" "{" "}" "<" ">"})
       (map {")" 1 "]" 2 "}" 3 ">" 4})
       (reduce (fn [acc score] (+ (* 5 acc) score)))))

(defn solve! [scores]
  (let [size (count scores)]
    (nth (sort scores) (Math/floor (/ size 2)))))

(defn part1 [lines]
  (->> (map examine lines)
       (filter #(= "corruptted" (get % :status)))
       (map :info)
       (map #({")" 3 "]" 57 "}" 1197 ">" 25137} (str (last %))))
       (reduce +)))

(defn part2 [lines]
  (->> (map examine lines)
       (filter #(= "incomplete" (get % :status)))
       (map :line)
       (map closing-score)
       solve!))

(comment
  ; => first attempt: too high 319386, second attempt (data reload, clear!): 319329
  (part1 input)

  ; => 3515583998
  (part2 input))

(deftest test-example
   (let [sample example]
     (is (= 26397 (part1 sample)))
     (is (= 288957 (part2 sample)))))
