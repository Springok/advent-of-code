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

(defn take-turns [monkeys nth-monkey state partx]
  (let [monkey (nth monkeys nth-monkey)
        items  (nth (:items state) nth-monkey)
        {:keys [op t-number tt tf]} monkey
        crt-product (reduce * (map :t-number monkeys))]
    (reduce (fn [state item]
              (let [w-level (if (= partx :part1)
                              (bigint (/ (op item) 3))
                              (bigint (mod (op item) crt-product)))
                    to-nth-monkey (if (zero? (mod w-level t-number)) tt tf)]
                (-> state
                    (update :items #(update (vec %) to-nth-monkey conj w-level))
                    (update :items #(update (vec %) nth-monkey rest))
                    (update :inspecting-times #(update % nth-monkey inc)))))
     state
     items)))

(defn take-rounds [monkeys turns initial-state partx]
  (loop [turns turns
         state initial-state]
    (if (= 0 (count turns))
      state
      (recur (rest turns) (take-turns monkeys (first turns) state partx)))))

(defn monkey-business [monkeys rounds partx]
  (let [init-state {:items (map :items monkeys)
                    :inspecting-times (vec (repeat (count monkeys) 0))}
        turns (take (* (count monkeys) rounds) (cycle (range (count monkeys))))]
   (->> (-> (take-rounds monkeys turns init-state partx)
            (:inspecting-times))
        (sort >)
        (take 2)
        (apply *))))

(defn part1 [input]
  (monkey-business (parse-monkeys input) 20 :part1))

(defn part2 [input]
  (monkey-business (parse-monkeys input) 10000 :part2))

(deftest test-example
  (is (= 10605 (part1 example)))
  (is (= 2713310158 (part2 example))))

(comment
  (time (part1 input))  ; 57348, "Elapsed time: 3.165958 msecs"
  (time (part2 input))) ; 14106266886, "Elapsed time: 39103.882917 msecs"
