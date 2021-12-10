; [Day 9 - Advent of Code 2021](https://adventofcode.com/2021/day/9)
(ns aoc.2021.day8
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.core.matrix :as mat]))

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

(defn check-min [[row col] h]
  (let [hmap heightmap
        count-row (dec (mat/row-count hmap))
        count-col (dec (mat/column-count hmap))
        adjacent-points (adjacent-points [row col] count-row count-col)
        ahs (map (fn [[row col]] (mat/mget hmap row col)) adjacent-points)]
    (if (every? #(< h %) ahs) (inc h) 0)))

(defn find-min [[row col] h]
  (let [hmap heightmap
        count-row (dec (mat/row-count hmap))
        count-col (dec (mat/column-count hmap))
        adjacent-points (adjacent-points [row col] count-row count-col)
        ahs (map (fn [[row col]] (mat/mget hmap row col)) adjacent-points)]
    (if (every? #(< h %) ahs) [row col] nil)))

(defn keep? [hmap [row col]]
  (> 9 (mat/mget hmap row col)))

(defn explore [hmap sets]
  (let [count-row (dec (mat/row-count hmap))
        count-col (dec (mat/column-count hmap))]
    (->> sets
      (reduce (fn [ac e] (apply conj ac (adjacent-points e count-row count-col))) #{})
      (filter #(keep? hmap %)))))

(defn visit-all-basin [start hmap]
  (loop [next-ps (explore hmap #{start})
         visited #{start}]
       (if (= visited (apply conj #{} (explore example visited)))
           visited
          (recur (explore hmap visited) (apply conj visited next-ps)))))

(comment
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
  (mat/mget heightmap 1 86)
  (mat/ecount example)
  (mat/emap-indexed (fn [[row col] a] [row col a]) example)

  (-> example)
  (mat/column-count (-> heightmap))
  (mat/row-count (-> example))

  ; part-1 example
  (mat/ereduce + (mat/emap-indexed check-min example))
  (mat/ereduce + (mat/emap-indexed check-min heightmap))
  (remove #(= 0 %) (mat/eseq (mat/emap-indexed check-min heightmap)))

  ; part-2 example
  (mat/select-indices example (vector [0 1] [0 9] [2 2] [4 6]))

  (let [hmap example
        indices (remove #(= 99 %) (mat/eseq (mat/emap-indexed find-min example)))]
      (->> indices
        (map #(visit-all-basin % hmap))
        (map count)
        (sort)
        (reverse)
        (take 3)
        (reduce *)))

  ; part-2
  (let [hmap heightmap
        matrix (mat/emap-indexed find-min heightmap)
        indices (reduce #(apply conj %1 (filter vector? %2)) [] matrix)]
      (first (->> indices)))
        ;; (map #(visit-all-basin % hmap))
        ;; (map count)
        ;; (sort)
        ;; (reverse)
        ;; (take 3)
        ;; (reduce *))))

  (visit-all-basin [0 0] heightmap))
