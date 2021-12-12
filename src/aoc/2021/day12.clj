; [Day 12 - Advent of Code 2021](https://adventofcode.com/2021/day/12)
(ns aoc.2021.day12
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.core.matrix :as mat]
    [clojure.set :as set]
    [clojure.test :refer [deftest is]]
    [loom.graph :as graph]
    [loom.gen :as gen]
    [loom.alg :as alg]
    [loom.alg-generic :as alg-gen]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day12.txt")))

(defn ->nodes [edges]
  (->> edges
    (map #(map keyword (str/split % #"-")))))

(defn path [cave-system start end path-visited])

(defn paths [cave-system start end]
  (loop [paths []
         idx 0]
     (if (= idx 10)
       paths
       (recur (conj paths (path cave-system start end paths)) (inc idx)))))

(defn part1 [edges]
  (prn edges))

(defn part2 [edges])

(comment)
    ; => first attempt: 1691
    ;; (part1 input 100))

    ; => 216
    ;; (part2 input 100))

(def example-1
  ["start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"])

(def example-2
  ["dc-end" "HN-start" "start-kj" "dc-start" "dc-HN" "LN-dc" "HN-end" "kj-sa" "kj-HN" "kj-dc"])

(def example-3
 ["fs-end" "he-DX" "fs-he" "start-DX" "pj-DX" "end-zg" "zg-sl" "zg-pj"
  "pj-he" "RW-he" "fs-DX" "pj-RW" "zg-RW" "start-pj" "he-WI" "zg-he"
  "pj-fs" "start-R"])

(def g1 ;; convert-to-map
  {:start [:A :b]
   :A [:c :b :end]
   :b [:A :d :end]
   :c [:A]
   :d [:b]})

(defn path-with-only-once-small-cave? [path small-caves]
  (->> small-caves
       (map (frequencies path))
       (remove nil?)
       (every? #(<= % 1))))


(comment
  (every? #(<= % 1) '(2))
  ;; try loom
  (let [graph (apply graph/graph (->nodes example-1))
        small-caves [:b :c :d]]
        ;; nodes (reduce-kv (fn [m k v] (assoc m k (vec v))) {} (:adj graph))]
     (count (filter #(path-with-only-once-small-cave? % small-caves) (alg-gen/trace-paths g1 :start)))))
     ;; graph))
     ;; (alg/bf-path-bi graph :start :end)))
     ;; graph
     ;; (graph/nodes graph)))

(deftest test-example
  (is (= 10 (part1 example-1))))
  ;; (is (= 19 (part1 example-2)))
  ;; (is (= 226 (part1 example-3))))
