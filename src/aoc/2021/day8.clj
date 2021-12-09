; [Day 8 - Advent of Code 2021](https://adventofcode.com/2021/day/8)
(ns aoc.2021.day8
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.set :as set]))

(defn sorted-segments [s]
 (apply sorted-set (str/split s #"")))

(defn notes [path]
 (->> (util/read-file-by-line path)
      (map #(str/split % #" \| "))))

(defn convert-to-sets [digits]
  (->> (str/split digits #" ")
       (map sorted-segments)))

(def decode-map-by-count
  {2 1
   3 7
   4 4
   7 8})

(defn decode-mapping-by-count [[patterns digits]]
  (let [sets (convert-to-sets (str patterns " " digits))]
    (reduce (fn [dmap s]
              (assoc dmap s (decode-map-by-count (count s) s)))
            {}
            sets)))

; having s1, s4
; [v] rule 2: 0, 6, 9 not contain 1 => 6
; [v] rule 3: 0, 9    contain 4 => 9 => 0
; [v] rule 1: 2, 3, 5 contain 1 => 3
; [v] rule 4: 2, 5    which included by 6 (superset) is 5

(defn decode-rest-mapping [dmap]
  (let [r-dmap (set/map-invert dmap)
        s1 (get r-dmap 1)
        s4 (get r-dmap 4)
        s7 (get r-dmap 7)
        s235 (filter #(= 5 (count %)) (keys dmap))
        s069 (filter #(= 6 (count %)) (keys dmap))
        s6 (first (filter #(and (not (set/superset? % s1)) (not (set/superset? % s4)) (not (set/superset? % s7))) s069))
        s5 (first (filter #(set/superset? s6 %) s235))
        s9 (first (filter #(set/superset? % s5) (remove #(= % s6) s069)))
        s3 (first (filter #(set/superset? s9 %) (remove #(= % s5) s235)))
        s0 (first (remove #(or (= % s6) (= % s9)) s069))
        s2 (first (filter #(= 4 (count (set/intersection s9 %))) (remove #(or (= % s3) (= % s5)) s235)))]
   (-> dmap
     (assoc s2 2)
     (assoc s3 3)
     (assoc s5 5)
     (assoc s0 0)
     (assoc s6 6)
     (assoc s9 9))))

(defn decode-mapping [[patterns digits]]
  (-> (decode-mapping-by-count [patterns digits])
      decode-rest-mapping))

(defn decode [[patterns digits]]
  (let [decode-map (decode-mapping [patterns digits])
        sets (convert-to-sets digits)]
     (map #(get decode-map %) sets)))

(comment
  (get {#{1 2} 1} #{1 2})
  (assoc (zipmap (range 0 10) (repeat 10 #{})) 1 #{23 12})
  (map #(str/split % #" \|") (map str/join (partition 2 (notes "../resources/aoc/2021/day8.txt")))) (sorted-segments "abce")

  ;; part 1 - example => 26
   (->> (notes "../resources/aoc/2021/day8.txt")
        (map decode)
        (map #(filter #{1 4 7 8} %))
        (flatten)
        (count))

  ;; part 2 - example => 61229
   (->> (notes "../resources/aoc/2021/day8.txt")
        (map decode)
        (map str/join)
        (map #(Integer/parseInt %))
        (reduce +))

  ;; part 1 => 476
   (->> (notes "../resources/aoc/2021/day8-part1.txt")
        (map decode)
        (map #(filter #{1 4 7 8} %))
        (flatten)
        (count))

  ;; part 2 => 1011823
   (->> (notes "../resources/aoc/2021/day8-part1.txt")
        (map decode)
        (map str/join)
        (map #(Integer/parseInt %))
        (reduce +)))
