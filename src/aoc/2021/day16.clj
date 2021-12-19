; [Day 16 - Advent of Code 2021](https://adventofcode.com/2021/day/16)
(ns aoc.2021.day16
  (:require
    [aoc.util :as util]
    [clojure.string :as str]
    [clojure.test :refer [deftest is]]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day16.txt")
       (first)))

(defn pad-zero [s n]
  (let [padding-zeros (reduce str (repeat (- n (count s)) "0"))]
    (str padding-zeros s)))

(def hex-mapping
  {"A" "10"
   "B" "11"
   "C" "12"
   "D" "13"
   "E" "14"
   "F" "15"})

(defn hex-to-binary [s]
  (->> (str/split s #"")
       (map #(get hex-mapping % %))
       (util/strs->integers)
       (map #(Integer/toBinaryString %))
       (mapcat #(pad-zero % 4))
       (apply str)))

(defn binary->int [s]
   (Integer/parseInt s 2))

(defn header [s]
  (->> [(subs s 0 3) (subs s 3 6)]
       (map binary->int)
       (zipmap [:VVV :TTT])))

(defn literal? [s]
  (= 4 (:TTT (header s))))

(defn parse-literal-body [body]
  (reduce (fn [result-binary [bit & four-bits :as five-bits]]
             (let [result (-> result-binary
                              (update :body #(apply conj % five-bits))
                              (update :four-bits #(apply conj % four-bits)))]
               (if (= \0 bit)
                 (reduced result)
                 result)))
          {:body [] :four-bits []}
          (partition 5 body)))

(defn literal-packet [s]
  (let [body (parse-literal-body (subs s 6))
        parsed-body (apply str (:body body))]
    (merge (header s)
           {:current-body parsed-body
            :next-lit-body (subs s (+ 6 (count parsed-body)))
            :content-decimal-number (binary->int (apply str (:four-bits body)))
            :p-length (count s)})))

(defn literal-packets [s]
 (loop [packet s
        data []]
   (let [{:keys [next-lit-body] :or {next-lit-body ""} :as result} (literal-packet packet)]
     (if (or (> 11 (count next-lit-body))
             (not (literal? next-lit-body)))
        (conj data result)
        (recur next-lit-body (conj data result))))))

(comment
  (literal-packet (hex-to-binary "D2FE28")))

(defn operator-label [s]
  (let [I (subs s 6 7)
        L-to (if (= I "0") (+ 15 7) (+ 11 7))
        number (binary->int (subs s 7 L-to))
        length (if (= I "0") number (* number 11))]
      {:I I
       :body-range [L-to (+ L-to length)]}))

(defn op-packet [s results]
 (if (literal? s)
   (let [results (apply conj results (literal-packets s))
         next-lit-body (get (last results) :next-lit-body "")]
    (if (< 11 (count next-lit-body))
      (op-packet next-lit-body results)
      results))
   (let [{:keys [body-range] :as label} (operator-label s)
         current-body   (apply subs s body-range)
         current-result (merge (header s) {:current-body current-body
                                           :p-length (count s)
                                           :label label})
         results (conj results current-result)]
     (if (not (literal? current-body))
       (op-packet (subs s (first body-range)) results)
       (let [results   (apply conj results (literal-packets current-body))
             next-body (subs s (last body-range))
             next-lit-body (get (last results) :next-lit-body "")]
         (cond->> results
           (< 11 (count next-lit-body)) (op-packet next-lit-body)
           (< 11 (count next-body)) (op-packet next-body)))))))

(defn part1 [input]
  (->> (op-packet (hex-to-binary input) [])
       (map :VVV)
       (apply +)))

(defn part2 [input])

(comment
  (literal-packet (hex-to-binary "D2FE28"))

  (op-packet (hex-to-binary "EE00D40C823060") []) ; operator contains 3 literal-packets
  (op-packet (hex-to-binary "38006F45291200") []) ; operator contains 2 literal-packets

  (count "00000000001")
  ; more examples 1 nested operator packet (ok)
  (op-packet (hex-to-binary "8A004A801A8002F478") [])

  (count "00101010000")

  ; more examples 2 (ok)
  (op-packet (hex-to-binary "620080001611562C8802118E34") [])

  ; more examples 3 (ok)
  (op-packet (hex-to-binary "C0015000016115A2E0802F182340") [])

  ; more examples 4 (ok)
  (op-packet (hex-to-binary "A0016C880162017C3686B18A3D4780") [])

  (count "010100000011001000001000110000011")
  (count "00000000011")

  (binary->int "001010101001100")


  (Integer/parseInt "011" 2)
  (subs "110100101111111000101000" 6 7)
  (pad-zero "10" 2)
  (format "%04d" 16)
  (type 0x0b)
  (Integer/toString 0x0b 2)
  (Integer/toBinaryString 14)
  (hex-to-binary input)

  (hex-to-binary "005532447836")
  ; 9730
  (op-packet (hex-to-binary input) [])

  (part1 input)

  ; 4110
  (part2 input))

; {version: 4 }
(deftest test-example
  (is (= "110100101111111000101000" (hex-to-binary "D2FE28")))
  (is (= "00111000000000000110111101000101001010010001001000000000" (hex-to-binary "38006F45291200")))

  (is (= 16 (part1 "8A004A801A8002F478")))
  (is (= 12 (part1 "620080001611562C8802118E34")))
  (is (= 23 (part1 "C0015000016115A2E0802F182340")))
  (is (= 31 (part1 "A0016C880162017C3686B18A3D4780"))))

  ;; (is (= 0 (part2 example))))

(comment
  "38006F45291200"
  "00111000000000000110111101000101001010010001001000000000"

  (binary->int "011111100101")
  (subs "11010001010" 6)
  (partition 5 "101111111000101000")

  "A" "110 100 01010"
  "B" "010 100 10001 00100")
