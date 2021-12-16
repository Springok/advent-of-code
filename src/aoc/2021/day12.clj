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

(defn small-caves [cave-system]
  (->> (keys cave-system)
       (remove #(or (= % :start) (= % :end)))
       (filter #(= (str %) (str/lower-case %)))
       (set)))

(defn visited? [path node]
  (some #(= node %) path))

(defn all-paths [cave-system small-caves path]
  (let [current (peek path)]
    (if (= current :end)
      (vector path)
      (->> (current cave-system)
           (remove #(and (small-caves %) (visited? path %)))
           (mapcat #(all-paths cave-system small-caves (conj path %)))))))

(defn remove-invalid-caves [small-caves node path]
  ;; remove when already have other nodes seen twice & this samll also seen
  (let [visited (frequencies path)
        seen-twice? (some (fn [[node times]] (when (and (small-caves node) (> times 1)) node)) visited)]
    (when (small-caves node)
      (and (visited? path node) seen-twice?))))

(defn all-paths2 [cave-system small-caves path]
  (let [current (peek path)]
    (if (= current :end)
      (vector path)
      (->> (current cave-system)
           (remove #(remove-invalid-caves small-caves % path))
           (mapcat #(all-paths2 cave-system small-caves (conj path %)))))))

(defn part1 [edges]
  (let [pairs (->pairs edges)
        cave-system (->cave-system pairs)
        small-caves (small-caves cave-system)]
    (->> (all-paths cave-system small-caves [:start])
         (count))))

(defn part2 [edges]
  (let [pairs (->pairs edges)
        cave-system (->cave-system pairs)
        small-caves (small-caves cave-system)]
    (->> (all-paths2 cave-system small-caves [:start])
         (count))))

(def example-1
  ["start-A" "start-b" "A-c" "A-b" "b-d" "A-end" "b-end"])

(def example-2
  ["dc-end" "HN-start" "start-kj" "dc-start" "dc-HN" "LN-dc" "HN-end" "kj-sa" "kj-HN" "kj-dc"])

(def example-3
 ["fs-end" "he-DX" "fs-he" "start-DX" "pj-DX" "end-zg" "zg-sl" "zg-pj"
  "pj-he" "RW-he" "fs-DX" "pj-RW" "zg-RW" "start-pj" "he-WI" "zg-he"
  "pj-fs" "start-RW"])

(comment
  ;; 3679
  (part1 input)

  ;; 107395
  (part2 input))

(deftest test-example
  (is (= 10 (part1 example-1)))
  (is (= 19 (part1 example-2)))
  (is (= 226 (part1 example-3)))

  (is (= 36 (part2 example-1)))
  (is (= 103 (part2 example-2)))
  (is (= 3509 (part2 example-3))))

(comment
  (select-keys {:as 1 :df 2 :adf 2} #{:df})
  (->> {:as 1 :df 2 :adf 2}
       (every? #(= (val %) 1)))
  (partition 2 1 [:c :b :d])
  (->cave-system (->pairs example-3)))
