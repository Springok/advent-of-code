; [Day 14 - Advent of Code 2021](https://adventofcode.com/2021/day/14)
(ns aoc.2021.day14
  (:require
    [aoc.util :as util]
    [clojure.set :as set]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2021/day14-ex.txt")))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day14.txt")))

(defn table [inputs]
  (->> inputs
       (drop 2)
       (mapcat #(str/split % #" -> "))
       (apply array-map)))

(defn pair-insertion [pair table]
  (let [c-pair (apply str pair)
        insert-e (table c-pair)]
    (apply str [(first pair) insert-e])))

(defn synthesis [structure table]
  (->> (partition 2 1 structure)
       (mapcat #(pair-insertion % table))
       (apply str)))

(defn grow [template table steps]
  (let [last-e (last template)]
    (loop [result template
           idx 0]
      (if (= idx steps)
        result
        (recur (str (synthesis result table) last-e) (inc idx))))))

; ====================================================================

(defn grow-2 [table pairs-freq]
  (reduce-kv (fn [m k v]
                (let [insert-e (get table k)]
                  (-> m
                      (update (str (first k) insert-e) (fnil + 0) v)
                      (update (str insert-e (last k)) (fnil + 0) v))))
             {}
             pairs-freq))

(defn grow-2-phase2
  "taking first is enough, since like NN -> NC, CN"
  [end pairs-freq]
  (reduce-kv (fn [m k v]
               (-> m
                   (update (str (first k)) (fnil + 0) v)))
             {(str end) 1}
             pairs-freq))

(defn synthesis-2 [structure table]
  (->> (partition 2 1 structure)
       (frequencies)
       (reduce-kv (fn [m k v]
                    (assoc m (apply str k) v))
                  {})
       (iterate (partial grow-2 table))
       (take (inc 40))
       (last)
       (grow-2-phase2 (last structure))))

(defn score [result]
  (- (val (apply max-key val result))
     (val (apply min-key val result))))

; ====================================================================

(defn part1 [input]
  (->> (grow (first input) (table input) 10)
       (frequencies)
       (score)))

(defn part2 [input]
   (let [structure (first input)
         table (table input)]
    (->> (synthesis-2 structure table)
         (score))))

(comment
  (part1 input)

  (part2 input))


(deftest test-example
  (is (= 1588 (part1 example)))
  (is (= 2188189693529 (part2 example))))

(comment
  (table example)
  (synthesis-2 "NNCB" (table example))

  (last "NNCB")
  (synthesis "NNCB" (table example))
  (part1 example)
  (pair-insertion "CH" {"CH" "B"})
  ({"CH" "B"} "CH")
  (->> (partition 2 1 "1232")))
