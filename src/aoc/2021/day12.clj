; [Day 12 - Advent of Code 2021](https://adventofcode.com/2021/day/12)
(ns aoc.2021.day12
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day12.txt")))

(defn ->pairs [edges]
  (->> edges
    (map #(map keyword (str/split % #"-")))))

(defn ->cave-system [pairs]
  (reduce (fn [m [from to]]
            (cond
              (= from :start) (-> m (update :start #(conj (vec %) to)))
              (= to :start)   (-> m (update :start #(conj (vec %) from)))
              (= from :end)   (-> m (update to #(conj (vec %) :end)))
              (= to :end)     (-> m (update from #(conj (vec %) :end)))
              :else (-> m
                        (update from #(conj (vec %) to))
                        (update to #(conj (vec %) from)))))
          {}
          pairs))

(defn visited? [path node]
  (some #(= node %) path))

(defn all-paths [cave-system small-caves path]
  (let [current (peek path)]
    (if (= current :end)
      (vector path)
      (->> (current cave-system)
           (remove #(and (small-caves %) (visited? path %)))
           (mapcat #(all-paths cave-system small-caves (conj path %)))))))

(comment
  (cons [:start :12] [:12])
  (partition 2 1 [:start]))

(defn small-caves [cave-system]
  (->> (keys cave-system)
       (remove #(or (= % :start) (= % :end)))
       (filter #(= (str %) (str/lower-case %)))
       (set)))

(defn part1 [edges]
  (let [pairs (->pairs edges)
        cave-system (->cave-system pairs)
        small-caves (small-caves cave-system)]
    (prn small-caves)
    (->> (all-paths cave-system small-caves [:start])
         (count))))

(defn part2 [edges])

(defn parse-graph [lines]
  (apply merge-with into
         (for [line lines
               :let [[a b] (str/split line #"-")]]
           {a [b], b [a]})))

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
  (part1 input)
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
