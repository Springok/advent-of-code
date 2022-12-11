(ns aoc.2022.day11
  (:require
    [aoc.util :as util]
    [clojure.string :as s]
    [clojure.test :refer [deftest is]]))

(def example
  (util/read-chunks "../resources/aoc/2022/day11-ex.txt"))

(def input
  (util/read-chunks "../resources/aoc/2022/day11.txt"))

(def parse-number
  (comp read-string (partial re-find #"\d+")))

(defn parse-op [op]
  (let [[op number] (map read-string (re-seq #"\*|\+|\-|\/|\d+" op))]
    (if (some? number)
      (partial (resolve op) number)
      (partial #(* % %)))))

(defn parse-monkey [[_ items op t-number tt tf]]
  {:items (map read-string (re-seq #"\d+" items))
   :op (parse-op op)
   :t-number (parse-number t-number)
   :tt (parse-number tt)
   :tf (parse-number tf)})

(defn parse-monkeys [input]
  (->> input
       (map s/split-lines)
       (map parse-monkey)))

(comment
  (let [f (parse-op "Operation: new = old * old")]
     (f 12))
  (parse-monkeys example)
  (parse-monkeys input))

(defn take-turn [monkeys nth-monkey state]
  (let [monkey (nth monkeys nth-monkey)
        items  (nth (:items state) nth-monkey)
        {:keys [op t-number tt tf]} monkey]
    (reduce (fn [state item]
              (let [w-level (bigint (/ (op item) 3))
                    to-nth-monkey (if (zero? (mod w-level t-number)) tt tf)]
                (-> state
                    (update :items #(update (vec %) to-nth-monkey conj w-level))
                    (update :items #(update (vec %) nth-monkey rest))
                    (update :inspecting-times #(update % nth-monkey inc)))))
     state
     items)))

(defn turns [monkeys rounds]
   (take (* (count monkeys) rounds) (cycle (range (count monkeys)))))

(defn init-state [monkeys]
  {:items (map :items monkeys)
   :inspecting-times (vec (repeat (count monkeys) 0))})

(defn process [monkeys turns initial-state]
  (loop [turns turns
         state initial-state]
    (let [nth-monkey (first turns)]
      (when (= nth-monkey 0) (prn "state" (:inspecting-times state)))
      (if (= 0 (count turns))
         state
         (recur (rest turns) (take-turn monkeys nth-monkey state))))))

(defn part1 [input]
  (let [monkeys (parse-monkeys input)
        init-state (init-state monkeys)
        turns (turns monkeys 20)]
    (->> (:inspecting-times (process monkeys turns init-state))
         (sort >)
         (take 2)
         (apply *))))

(defn part2 [input])

(deftest test-example
  (is (= 10605 (part1 example)))
  (is (= 2713310158 (part2 example))))

(comment
  (time (part1 input))  ; 57348, "Elapsed time: 3.165958 msecs"
  (time (part2 input))) ; "Elapsed time: 1.037208 msecs"
