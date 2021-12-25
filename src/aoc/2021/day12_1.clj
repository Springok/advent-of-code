; [Day 12 - Advent of Code 2021](https://adventofcode.com/2021/day/12)
(ns aoc.2021.day12-1
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day12.txt"))
  (->> (util/read-file "../resources/aoc/2021/day12.txt")))

(defn parse-graph [lines]
  (apply merge-with into
         (for [line lines
               :let [[a b] (str/split line #"-")]]
           {a [b], b [a]})))

(defn small-cave? [s] (Character/isLowerCase (first s)))

(defn ^:blog dfs-paths [g goal path allowances]
  (let [curr (peek path)]
    (if (= goal curr)
      (vector path)
      (let [nexts (filter #(pos? (get allowances %)) (get g curr))]
        (mapcat #(dfs-paths g goal (conj path %) (update allowances curr dec)) nexts)))))

(defn ^:blog make-allowances
  "Returns map of cave to number of times it may be visited.
  Small caves begin with lowercase and can be visited once.
  Large caves (everything not small) can be visited infinitely."
  [g]
  (let [{small true, big false} (group-by small-cave? (keys g))]
    (merge (zipmap small (repeat 1)) (zipmap big (repeat ##Inf)))))

(defn part-1 [input]
  (let [g     (parse-graph input)
        allow (make-allowances g)]
    (count (dfs-paths g "end" ["start"] allow))))


(defn ^:blog part-2 [input]
  (let [g           (parse-graph input)
        init-allow  (make-allowances g)
        small-caves (remove #{"start" "end"} (filter small-cave? (keys g)))]
    (->> small-caves
         (map #(update init-allow % inc))
         (mapcat (partial dfs-paths g "end" ["start"]))
         set
         count)))

(def example-1
  ["start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"])

(def example-2
  ["dc-end" "HN-start" "start-kj" "dc-start" "dc-HN" "LN-dc" "HN-end" "kj-sa" "kj-HN" "kj-dc"])

(def example-3
 ["fs-end" "he-DX" "fs-he" "start-DX" "pj-DX" "end-zg" "zg-sl" "zg-pj"
  "pj-he" "RW-he" "fs-DX" "pj-RW" "zg-RW" "start-pj" "he-WI" "zg-he"
  "pj-fs" "start-R"])

(comment
  (partition 2 1 [:c :b :d])
  (part-1 input)
  (->cave-system (->pairs example-3))
  (parse-graph example-3))

    ; => first attempt: 1691
    ;; (part1 input 100))

    ; => 216
    ;; (part2 input 100))


(deftest test-example
  ;; (is (= 10 (part1 example-1)))
  ;; (is (= 19 (part1 example-2)))
  (is (= 226 (part1 example-3))))
