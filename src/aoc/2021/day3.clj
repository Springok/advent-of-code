; [Day 3 - Advent of Code 2021](https://adventofcode.com/2021/day/3)
(ns aoc.2021.day3
  (:require
    [aoc.util :as util]
    [clojure.string :as string]
    [clojure.test :refer [deftest is]]))

(def example
  ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day3.txt")))

(defn transfrom-array [array]
  (for [n (range (count (first array)))]
    (map #(nth % n) array)))

(defn rate-in-binary [report]
  (->> report
       (map #(string/split % #""))
       (transfrom-array)
       (map frequencies)
       (map #(sort-by val > %))
       (map ffirst)
       (apply str)))

(defn epsilon [gamma]
  (->> (string/split gamma #"")
       (map #(if (= % "0") "1" "0"))
       (apply str)))

(defn part1 [input]
  (let [gamma (rate-in-binary input)
        epsilon (epsilon gamma)]
       (->> [gamma epsilon]
            (mapv #(Integer/parseInt % 2))
            (apply *))))

(defn find-bit [check-type m]
  (let [common-bit (if (= "ox" check-type) "1" "0")
        op (if (= "ox" check-type) > <)]
    (if (apply = (vals m))
       common-bit
       (ffirst (sort-by val op m)))))

(defn bit [report n check-type]
  (->> report
       (map #(string/split % #""))
       (map #(nth % n))
       (frequencies)
       (find-bit check-type)))

(defn nth-chr [s n]
  (nth (string/split s #"") n))

(defn filtered-report [report n check-type]
  (let [bit (bit report n check-type)]
    (if (= 1 (count report))
      report
      (filter #(= (nth-chr % n) bit) report))))

(defn part2 [input]
  (->> (for [check-type ["ox" "co2"]]
         (->> (drop 1 (reductions #(filtered-report %1 %2 check-type) input (range (count (first input)))))
              (map-indexed #(nth-chr (first %2) %1))
              (apply str)))
       (mapv #(Integer/parseInt % 2))
       (apply *)))

(comment
  (part1 input) ;; => 749376
  (part2 input)) ;; => 2372923

(deftest test-example
   (let [sample example]
     (is (= 198 (part1 sample)))
     (is (= 230 (part2 sample)))))
