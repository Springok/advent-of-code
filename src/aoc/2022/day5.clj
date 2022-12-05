(ns aoc.2022.day5
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example-stacks
  {1 [\Z \N]
   2 [\M \C \D]
   3 [\P]})

(def puzzle-stacks
  {1 [\V \C \D \R \Z \G \B \W]
   2 [\G \W \F \C \B \S \T \V]
   3 [\C \B \S \N \W]
   4 [\Q \G \M \N \J \V \C \P]
   5 [\T \S \L \F \D \H \B]
   6 [\J \V \T \W \M \N]
   7 [\P \F \L \C \S \T \G]
   8 [\B \D \Z]
   9 [\M \N \Z \W]})

(def example-moves
  (->> (util/read-file-by-line "../resources/aoc/2022/day5-ex.txt")))

(def input-moves
  (->> (util/read-file-by-line "../resources/aoc/2022/day5.txt")))

(defn parse-moves [input]
  (->> input
      (map #(re-seq #"\d+" %))
      (map #(map read-string %))))

(defn move [mover-fn stacks [n-crates from to]]
  (let [crates (take-last n-crates (get stacks from))]
    (-> stacks
      (update from #(drop-last n-crates %))
      (update to mover-fn crates))))

(defn mover-9000 [stack crates]
  (concat stack (reverse crates)))

(defn mover-9001 [stack crates]
  (concat stack crates))

(defn solve [mover-type stacks moves]
  (let [mover-fn (if (= :9001 mover-type) mover-9001 mover-9000)]
    (->> (reduce (partial move mover-fn) stacks (parse-moves moves))
         (into (sorted-map))
         (vals)
         (map last)
         (apply str))))

(defn part1 [stacks moves]
  (solve :9000 stacks moves))

(defn part2 [stacks moves]
  (solve :9001 stacks moves))

(deftest test-example
  (is (= "CMZ" (part1 example-stacks example-moves)))
  (is (= "MCD" (part2 example-stacks example-moves))))

(comment
  (time (part1 puzzle-stacks input-moves))   ; TBVFVDZPN => "Elapsed time: 1.60875 msecs"
  (time (part2 puzzle-stacks input-moves)))  ; VLCWHTDSZ => "Elapsed time: 1.858916 msecs"
