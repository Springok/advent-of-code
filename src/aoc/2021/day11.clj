; [Day 11 - Advent of Code 2021](https://adventofcode.com/2021/day/11)
(ns aoc.2021.day11
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.core.matrix :as mat]
    [clojure.set :as set]
    [clojure.test :refer [deftest is]]))

(defn strs->integers [strs]
   (map #(Integer/parseInt %) strs))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2021/day11-ex.txt")
       (map #(str/split % #""))
       (map strs->integers)))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day11.txt")
       (map #(str/split % #""))
       (map strs->integers)))

(defn level-zeros [level-map]
  (->> (mat/emap-indexed (fn [[row col] level]
                           (when (= 0 level) [row col]))
                         level-map)
       (mapcat #(filter vector? %))
       (set)))

(defn flash [level-map]
  (mat/emap-indexed (fn ([_idx level]
                         (if (= level 9) 0 (inc level))))
                    level-map))

(defn adjacents [[row col] count-row count-col]
  (->> [[(dec row) col] [(inc row) col]
        [row (dec col)] [row (inc col)]
        [(dec row) (dec col)] [(inc row) (inc col)]
        [(inc row) (dec col)] [(dec row) (inc col)]]
       (filter (fn [[arow acol]]
                 (and (>= count-row arow 0)
                      (>= count-col acol 0))))
       (set)))

(defn light-emit [level-map new-flashes]
  (let [count-r (dec (mat/row-count level-map))
        count-c (dec (mat/column-count level-map))]
    (reduce (fn [lmap flash]
              (let [flashes (adjacents flash count-r count-c)
                    flashed (level-zeros lmap)]
                (mat/emap-indexed (fn [idx level]
                                    (if (and (flashes idx) (not (flashed idx)))
                                      (if (= level 9) 0 (inc level))
                                      level))
                                  lmap)))
            level-map
            new-flashes)))

(defn sync? [level-map]
  (mat/zero-matrix? level-map))

(defn step [octopuses]
  (loop [flashed #{}
         level-map (flash (:level-map octopuses))
         idx 0]
    (let [new-flashes (set/difference (level-zeros level-map) flashed)]
      (if (empty? new-flashes)
         (-> octopuses
             (assoc :level-map level-map)
             (update :steps inc)
             (update :flashes #(+ % (count flashed))))
         (recur (apply conj flashed new-flashes) (light-emit level-map new-flashes) (inc idx))))))

(defn part1 [octopuses steps]
  (->> (iterate step {:steps 0 :flashes 0 :level-map octopuses})
       (take (inc steps))
       (last)
       (:flashes)))

(defn part2 [octopuses]
  (loop [octopuses {:steps 0 :flashes 0 :level-map octopuses}]
    (let [steps (:steps octopuses)
          level-map (:level-map octopuses)]
      (if (sync? level-map)
        steps
        (recur (step octopuses))))))

(comment
  ; => first attempt: 1691
  (part1 input 100)

  ; => 216
  (part2 input))

(deftest test-example
  (let [sample example]
    (is (= 35 (part1 sample 2)))
    (is (= 204 (part1 sample 10)))
    (is (= 1656 (part1 sample 100)))))

(comment
  (#{[1 12] [2 3]} [2 3])
  (empty? #{})
  (set/difference #{2 3} #{2 3})
  (-> example)
  (adjacents [1 1] 10 10)
  (step {:steps 0 :flashes 0 :level-map example})
  (level-zeros example)
  (flash (flash example)))
