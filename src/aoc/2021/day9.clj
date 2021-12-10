; [Day 9 - Advent of Code 2021](https://adventofcode.com/2021/day/9)
(ns aoc.2021.day9
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.core.matrix :as mat]
    [clojure.test :refer [deftest is]]))

(defn strs->integers [strs]
   (map #(Integer/parseInt %) strs))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2021/day9-ex.txt")
       (map #(str/split % #""))
       (map strs->integers)))

(def heightmap
  (->> (util/read-file-by-line "../resources/aoc/2021/day9.txt")
       (map #(str/split % #""))
       (map strs->integers)))

(defn adjacent-points [[row col] count-row count-col]
  (->> [[(dec row) col]
        [(inc row) col]
        [row (dec col)]
        [row (inc col)]]
       (filter (fn [[arow acol]]
                 (and (>= count-row arow 0)
                      (>= count-col acol 0))))))

(defn around-heights [[row col] hmap]
  (let [count-row (dec (mat/row-count hmap))
        count-col (dec (mat/column-count hmap))
        adjacent-points (adjacent-points [row col] count-row count-col)]
    (map (fn [[row col]] (mat/mget hmap row col)) adjacent-points)))

(defn keep? [hmap [row col]]
  (not= 9 (mat/mget hmap row col)))

(defn explore [hmap sets]
  (let [count-row (dec (mat/row-count hmap))
        count-col (dec (mat/column-count hmap))]
    (->> sets
      (reduce (fn [ac e] (apply conj ac (adjacent-points e count-row count-col))) #{})
      (filter #(keep? hmap %))
      (apply conj #{}))))

(defn visit-all-basin [start hmap]
  (loop [next-ps (explore hmap #{start})
         visited #{start}
         idx 0]
      (if (= visited (explore hmap visited))
          visited
         (recur (explore hmap visited) (apply conj visited next-ps) (inc idx)))))

(defn part1 [hmap]
  (let [low-heights (mat/emap-indexed (fn [[row col] h]
                                        (if (every? #(< h %) (around-heights [row col] hmap))
                                          (inc h)
                                          0))
                                      hmap)]
       (->> low-heights
           (mat/ereduce +))))

(defn part2 [hmap]
  (let [low-points (mat/emap-indexed (fn [[row col] h]
                                       (if (every? #(< h %) (around-heights [row col] hmap))
                                         [row col]
                                         nil))
                                     hmap)
        low-points (filter #(vector? %) (mat/eseq low-points))]
       (->> low-points
            (map #(visit-all-basin % hmap))
            (sort-by count >)
            (take 3)
            (map count)
            (reduce *))))
(comment
  ;; => 585
  (part1 heightmap)

  ;; => 827904
  (part2 heightmap))

(deftest test-example
   (let [sample example]
     (is (= 15 (part1 sample)))
     (is (= 1134 (part2 sample)))))

(comment
  ; test around
  (not= 9 (mat/mget heightmap 1 0))
  (visit-all-basin [0 0] heightmap)
  (explore heightmap #{[0 20]})
  (mat/mget heightmap 1 86)

  (conj #{} [1 2])
  (visit-all-basin [0 9] example)
  (mat/select-indices example (visit-all-basin [0 9] example))
  (apply conj #{} (explore example #{[1 9] [0 9] [0 8]}))
  (keep? example [0 1])
  (mat/select-indices example (->> (reduce (fn [ac e] (apply conj ac (adjacent-points e 4 9))) #{} [[2 2]])
                               (filter #(keep? example %))))

  (filter #(keep? example %) #{[0 1]})
  (reduce (fn [ac e] (conj ac e)) #{} (apply conj #{} [1 2 3]))

  (adjacent-points [0 0] 4 9)
  (mat/ecount example)
  (mat/emap-indexed (fn [[row col] a] [row col a]) example)

  (-> example)
  (mat/column-count (-> heightmap))
  (mat/row-count (-> example)))
