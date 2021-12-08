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

(def default-segments
   (->> (map-indexed (fn [idx s] [(sorted-segments s) idx])
                     ["abcefg" "cf" "acdeg" "acdfg" "bcdf" "abdfg" "abdefg" "acf" "abcdefg" "abcdfg"])
        (flatten)
        (apply array-map)))
  ;; {"a" #{0 2 3 5 6 7 8 9}
  ;;  "b" #{0 4 5 6 8 9}
  ;;  "c" #{0 1 2 3 4 7 8 9}
  ;;  "d" #{2 3 4 5 6 8 9}
  ;;  "e" #{0 2 6 8}
  ;;  "f" #{0 1 3 4 5 6 7 8 9}
  ;;  "g" #{0 2 3 5 6 8 9}})

(defn convert-to-sets [digits]
  (->> (str/split digits #" ")
       (map sorted-segments)))

(def decode-map-by-count
  {2 1
   3 7
   4 4
   ;; 5 [2 3 5]
   ;; 6 [0 6 9]
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

(defn find-069 [dmap]
  (let [r-dmap (set/map-invert dmap)
        s1 (get r-dmap 1 #{"99"})
        s4 (get r-dmap 4 #{"99"})
        s7 (get r-dmap 7 #{"99"})
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
      find-069))

(defn decode [[patterns digits]]
  (let [decode-map (decode-mapping [patterns digits])
        sets (convert-to-sets digits)]
     ;; decode-map
     (map #(get decode-map %) sets)))

(defn decode-by-count [[patterns digits]]
  (let [decode-map (decode-mapping-by-count [patterns digits])
        sets (convert-to-sets digits)]
     (map #(get decode-map %) sets)))

(comment
  (get {#{1 2} 1} #{1 2})
  (assoc (zipmap (range 0 10) (repeat 10 #{})) 1 #{23 12})

  (map #(str/split % #" \|") (map str/join (partition 2 (notes "../resources/aoc/2021/day8.txt")))) (sorted-segments "abce")

  ;; example
  (let [notes (notes "../resources/aoc/2021/day8.txt")]
   (->> notes
       (map decode-by-count)
       (map #(filter #{1 4 7 8} %))
       (flatten)
       (count)))

  ;; part 2 - example
  (let [notes (notes "../resources/aoc/2021/day8.txt")]
   (->> notes
      (map decode)
      (map str/join)
      (map #(Integer/parseInt %))
      (reduce +)))

  ;; part 1
  (let [notes (notes "../resources/aoc/2021/day8-part1.txt")]
   (->> notes
       (map decode-by-count)))
       ;; (map #(filter #{1 4 7 8} %))
       ;; (flatten)
       ;; (count)))

  ;; part 2
  (let [notes (notes "../resources/aoc/2021/day8-part1.txt")]
   (->> (nth notes 6)))
      ;; decode))

  (let [notes (notes "../resources/aoc/2021/day8-part1.txt")]
   (->> notes
      (map decode)
      (map str/join))))
      ;; (map #(Integer/parseInt %))
      ;; (reduce +))))
