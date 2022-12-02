(ns aoc.util
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

;; reference https://github.com/tschady/advent-of-code/blob/main/src/aoc/file_util.clj

(defn read-file
  "Return full file contents from `path`."
  [path]
  (-> path io/resource slurp))

(defn read-chunks
  "Return file contents as collection of chunks, where chunks are separated by a
  full blank line."
  [path]
  (-> path read-file (str/split #"\n\n")))

(defn read-file-by-line
  [path]
  (-> path read-file str/split-lines))

(defn board
  "Return a vector contains n x n size of input"
  [input]
  (let [board (->> input
                   (mapv (fn [line] (mapv #(Integer/parseInt (str %)) line))))]
    board))

(defn ->integers
  "Return a collection of integers found in a string.  Integers may be negative."
  [s]
  (map read-string (re-seq #"-?\d+" s)))

(defn strs->integers
  "Return a collection of integers by parsing each str"
  [strs]
  (map #(Integer/parseInt %) strs))

(comment
  (->integers "12 123 123123 12312")
  (read-file "../resources/aoc/2021/day4.txt")
  (read-file-by-line "../resources/aoc/2021/day4.txt")
  (read-chunks "../resources/aoc/2021/day4.txt")
  (board (read-file-by-line "../resources/aoc/2021/day15-ex.txt"))
  (map str/trim-newline (read-chunks "../resources/aoc/2021/day4.txt"))
  (map #(str/join " " %) (map str/split-lines (read-chunks "../resources/aoc/2021/day4.txt"))))
