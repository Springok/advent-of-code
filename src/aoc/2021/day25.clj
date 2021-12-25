; [Day 25 - Advent of Code 2021](https://adventofcode.com/2021/day/25)
(ns aoc.2021.day25
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
 ["v...>>.vv>"
  ".vv>>.vv.."
  ">>.>v>...v"
  ">>v>>.>.v."
  "v>v.vv.v.."
  ">.>>..v..."
  ".vv..>.>v."
  "v.v..>>v.v"
  "....v..v.>"])


(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day25.txt")))

(defn parse [input]
  (reduce-kv (fn [m idx-r row]
               (reduce
                 (fn [m [sc r c]] ;; sc => sea cucumber
                   (update m sc conj [r c]))
                 m
                 (map-indexed #(vector %2 idx-r %1) row)))
    {\v (sorted-set)
     \> (sorted-set)
     \. (sorted-set)
     :rightmost (dec (count (first input)))
     :southmost (dec (count input))}
    input))


(defn move [direction {:keys [rightmost southmost] :as sc-map}]
  (reduce (fn [m [r c :as pos]]
            (let [next-pos (case direction
                             \> (if (= c rightmost) [r 0] [r (inc c)])
                             \v (if (= r southmost) [0 c] [(inc r) c]))]
              (if ((get sc-map \.) next-pos)
                (-> m
                    (update \. conj pos)
                    (update \. disj next-pos)
                    (update direction disj pos)
                    (update direction conj next-pos))
                m)))
          sc-map
          (get sc-map direction)))

(defn part1 [input]
  (loop [sc-map (parse input)
         idx 1]
    (let [next-map (->> sc-map
                        (move \>)
                        (move \v))]
      (if (= sc-map next-map)
        (-> (assoc sc-map :step idx)
            (:step))
        (recur next-map (inc idx))))))


(comment
  (part1 (parse input)) ; => 329

  (move \> (parse example))
  (time (part1 example))
  (time (part1 input))

  (range 10)
  (update {\v (sorted-set) \> #{} \. #{}} \v conj [1 2])
  (parse example)
  (conj #{} [1 2]))

(deftest test-example
  (is (= 58 (part1 example))))
