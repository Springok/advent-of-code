; [Day 17 - Advent of Code 2021](https://adventofcode.com/2021/day/17)
(ns aoc.2021.day17
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
  ["Player 1 starting position: 4" "Player 2 starting position: 8"])

(def input
  ["Player 1 starting position: 10" "Player 2 starting position: 2"])

(defn parse [input]
  (->> (util/->integers input)))


(defn part1 [[p1-start-pos p2-start-pos]]
  ; one outcome
  (let [result (reduce (fn [{:keys [p1-pos p2-pos] :as result} [p1-dices p2-dices]]
                        (let [p1-get (rem (+ p1-pos p1-dices) 10)
                              p2-get (rem (+ p2-pos p2-dices) 10)
                              p1-get (if (= p1-get 0) 10 p1-get)
                              p2-get (if (= p2-get 0) 10 p2-get)
                              result (-> result
                                         (assoc :p1-pos p1-get)
                                         (assoc :p2-pos p2-get)
                                         (update :p1-score #(+ % p1-get))
                                         (update :p2-score #(+ % p2-get))
                                         (update :round inc))]
                          (if (<= 1000 (:p1-score result))
                            (reduced (-> result
                                         (assoc :p2-pos p2-pos)
                                         (update :p2-score #(- % p2-get))))
                            result)))
                      {:p1-score 0 :p2-score 0 :p1-pos p1-start-pos :p2-pos p2-start-pos :round 0}
                      (->> (range 1 (inc 100))
                           (repeat)
                           (apply concat)
                           (partition 3)
                           (map #(apply + %))
                           (partition 2 2)))]
      (* (:p2-score result) (- (* 6 (:round result)) 3))))

(defn part2 [input])
  ; three dices (1, 2, 3)
  ; multiple outcomes

(comment
  (some #(= % 1000) [1000 813])
  (part1 [4 8])
  (part1 [10 2])
  (part1 input)

  (part2 input)

  (->> (range 1 (inc 100))
       (repeat)
       (apply concat)
       (partition 3)
       (map #(apply + %))
       (partition 2 2)))

(deftest test-example
  (is (= 739785 (part1 [4 8])))
  (is (= 444356092776315 (part2 example))))

(comment
  (rem 12 11)
  (util/->integers input)
  (->> (partition 3 3 (range 1 (inc 100)))
       (map #(apply + %))
       (partition 2 1))
  [(range 20 (inc 30)) (range -10 (inc -5))]
  (apply min (range 20 (inc 30)))
  (part1 example))
