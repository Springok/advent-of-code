; [Day 16 - Advent of Code 2021](https://adventofcode.com/2021/day/16)
(ns aoc.2021.day16-1
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day16.txt")
       (first)))

(def hex-table
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn to-bits [s]
  (->> (seq s)
       (map #(get hex-table % %))
       (apply str)))

(defn binary->int [s]
   (Integer/parseInt s 2))

; 1. memo
(declare parse-packet)

(defn take-bits [bits length]
  [(subs bits 0 length) (subs bits length)])

(defn parse-bits [bits length]
  (let [[v bits] (take-bits bits length)]
     [(binary->int v) bits]))

(defn parse-literal [bits]
  (loop [value 0
         bits bits]
    (let [[flag bits] (parse-bits bits 1)
          [chunk bits] (parse-bits bits 4)
          value (+ chunk (bit-shift-left value 4))]
      (case flag
        0 [value bits]
        1 (recur value bits)))))

(comment
  (parse-literal (to-bits "D2FE28"))
  (parse-literal "101111111000101"))

(defn parse-length-packets [length bits]
  (let [[bits rest] (take-bits bits length)]
    (loop [packets []
           bits bits]
      (if (zero? (count bits))
        [packets rest]
        (let [[packet bits] (parse-packet bits)]
           (recur (conj packets packet) bits))))))

(defn parse-count-packets [n bits]
  (loop [packets []
         bits bits]
    (if (= n (count packets))
      [packets bits]
      (let [[packet bits] (parse-packet bits)]
        (recur (conj packets packet) bits)))))

(defn parse-packets [bits]
  (let [[length-type-id bits] (parse-bits bits 1)
        [packets-parser length] (case length-type-id
                                        0 [parse-length-packets 15]
                                        1 [parse-count-packets 11])
        [n bits] (parse-bits bits length)]
     (packets-parser n bits)))

(defn parse-packet [bits]
  (let [[version bits] (parse-bits bits 3)
        [type-id bits] (parse-bits bits 3)
        body-parser (if (= 4 type-id) parse-literal parse-packets)
        [body bits] (body-parser bits)]
    [{:version version
      :type-id type-id
      :body body}
     bits]))

(defn sum-version [{:keys [version type-id body]}]
  (case type-id
    4 version
    (->> body (map sum-version) (reduce + version))))

(def bool->int {true 1 false 0})

(defn evaluate [{:keys [type-id body]}]
  (case type-id
    4 body
    0 (->> body (map evaluate) (reduce +))
    1 (->> body (map evaluate) (reduce *))
    2 (->> body (map evaluate) (reduce min))
    3 (->> body (map evaluate) (reduce max))
    5 (->> body (map evaluate) (reduce >) bool->int)
    6 (->> body (map evaluate) (reduce <) bool->int)
    7 (->> body (map evaluate) (reduce =) bool->int)))

(defn part1 [input]
  (-> input
      to-bits
      parse-packet
      first
      sum-version))

(defn part2 [input]
  (-> input
      to-bits
      parse-packet
      first
      evaluate))

(comment
  (part1 "D2FE28")
  (part1 input) ; => 981
  (time (part2 input))) ; => 981

(deftest test-example
  (is (= "110100101111111000101000" (to-bits "D2FE28")))
  (is (= "00111000000000000110111101000101001010010001001000000000" (to-bits "38006F45291200")))

  (is (= 16 (part1 "8A004A801A8002F478")))
  (is (= 12 (part1 "620080001611562C8802118E34")))
  (is (= 23 (part1 "C0015000016115A2E0802F182340")))
  (is (= 31 (part1 "A0016C880162017C3686B18A3D4780")))

  (is (= 3 (part2 "C200B40A82")))
  (is (= 54 (part2 "04005AC33890")))
  (is (= 7 (part2 "880086C3E88112")))
  (is (= 9 (part2 "CE00C43D881120")))
  (is (= 1 (part2 "D8005AC2A8F0")))
  (is (= 0 (part2 "F600BC2D8F")))
  (is (= 0 (part2 "9C005AC2F8F0")))
  (is (= 1 (part2 "9C0141080250320F1802104A08"))))
