; [Day 21 - Advent of Code 2021](https://adventofcode.com/2021/day/21)
(ns aoc.2021.day21
  (:require
    [clojure.test :refer [deftest is]]))

(def example
  [4 8])

(def input
  [10 2])

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

(def roll-counts
  (->> (for [a (range 1 4)
             b (range 1 4)
             c (range 1 4)]
         (+ a b c))
       frequencies))

(def wins
  (memoize
   (fn [[p1 p2] [s1 s2] first?]
     (->> (for [[roll n] roll-counts]
            (let [np (mod (+ p1 roll) 10)
                  ns (+ s1 np 1)]
              (if (<= 21 ns)
                {first? n}
                (-> (wins [p2 np] [s2 ns] (not first?))
                    (update-vals #(* n %))))))
          (reduce (partial merge-with +))))))

(defn part2 [input]
  (prn input)
  (->> (wins input [0 0] true)
       vals
       (reduce max)))

(deftest test-example
  (is (= 739785 (part1 example)))
  (is (= 444356092776315 (part2 example))))

(comment
  (time (part1 input)) ; 916083, "Elapsed time: 1.0745 msecs"
  (time (part2 input))) ; 49982165861983, "Elapsed time: 0.187959 msecs"
