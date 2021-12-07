(ns aoc.util
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]))

;; reference https://github.com/tschady/advent-of-code/blob/main/src/aoc/file_util.clj

(defn read-file
  "Return full file contents from `path`."
  [path]
  (-> path io/resource slurp str/trim-newline))

(defn read-chunks
  "Return file contents as collection of chunks, where chunks are separated by a
  full blank line."
  [path]
  (-> path read-file (str/split #"\n\n")))

(defn read-file-by-line
  [path]
  (-> path read-file str/split-lines))

(defn ->integers
  "Return a collection of integers found in a string.  Integers may be negative."
  [s]
  (map read-string (re-seq #"-?\d+" s)))

(comment
  (->integers "12 123 123123 12312")
  (read-file "../resources/aoc/day4.txt")
  (read-file-by-line "../resources/aoc/day4.txt")
  (read-chunks "../resources/aoc/day4.txt")
  (map #(str/join " " %) (map str/split-lines (read-chunks "../resources/aoc/day4.txt")))

  (map str/trim-newline (read-chunks "../resources/aoc/day4.txt")))
