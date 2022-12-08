(ns aoc.2022.day8
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2022/day8-ex.txt")
       (util/board)))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day8.txt")
       (util/board)))

; helper functions
(defn get-h [h-map [i j]]
  (nth (nth h-map j) i))

(defn trees-height-in-4-directions [h-map [i j]]
  (let [i-count (count (first h-map))
        j-count (count h-map)
        sn-trees (for [j1 (range j-count)] [i j1])
        ew-trees (for [i1 (range i-count)] [i1 j])
        [e-trees w-trees] (split-at i (map #(get-h h-map %) ew-trees))
        [n-trees s-trees] (split-at j (map #(get-h h-map %) sn-trees))
        w-trees (drop 1 w-trees)
        s-trees (drop 1 s-trees)]
   [e-trees w-trees n-trees s-trees]))

(defn visibile? [h-map [i j]]
  (let [current-h (get-h h-map [i j])]
    (->> (trees-height-in-4-directions h-map [i j])
         (map (fn [trees] (every? #(< % current-h) trees)))
         (some true?))))

(defn visibility-map [input]
  (let [i-count (count (first input))
        j-count (count input)]
    (for [i (range i-count)
          j (range j-count)]
      (if (or (= i 0) (= i (dec i-count))
              (= j 0) (= j (dec j-count))
              (visibile? input [i j]))
        1 0))))

(comment
  (get-h input [0 0])
  (get-h example [4 3])
  (visibile? example [1 1])
  (visibile? example [2 3]))

(defn see-trees [h tree-hs]
  (loop [hs tree-hs
         trees 0]
    (if (or (empty? hs) (<= h (first hs)))
      (if (empty? hs) trees (inc trees))
      (recur (drop 1 hs) (inc trees)))))

(defn scenic-score [h-map [i j]]
  (let [current-h (get-h h-map [i j])
        [e-trees w-trees n-trees s-trees] (trees-height-in-4-directions h-map [i j])]
    (->> [(reverse e-trees) w-trees (reverse n-trees) s-trees]
         (map #(see-trees current-h %))
         (apply *))))

(defn scenic-score-map [input]
  (let [i-count (count (first input))
        j-count (count input)]
    (for [i (range i-count)
          j (range j-count)]
      (scenic-score input [i j]))))

(comment
  (scenic-score example [2 1])
  (scenic-score example [2 3])
  (see-trees 5 [4 9])
  (partition 5 (scenic-score-map example)))

(defn part1 [input]
  (->> (visibility-map input)
       (apply +)))

(defn part2 [input]
  (->> (scenic-score-map input)
       (apply max)))

(deftest test-example
  (is (= 21 (part1 example)))
  (is (= 519064 (part2 input))))

(comment
  (time (part1 input))  ; 1546, "Elapsed time: 228.158792 msecs"
  (time (part2 input))) ; 519064, "Elapsed time: 243.897416 msecs"
