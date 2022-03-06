(ns aoc.misc.quicksort
  (:require
    [clojure.test :refer [deftest is]]))

(defn quicksort [items]
  (if (< (count items) 2)
    items
    (let [pivot (first items)
          less (filter #(< % pivot) items)
          great (filter #(> % pivot) items)]
      (concat (quicksort less) (vector pivot) (quicksort great)))))

(comment
  (filter #(< % 2) [12 123 111 1])
  (concat [1 2] (vector 23) [1 34])
  (quicksort [1 2 4 5]))

(deftest test-quicksort
  (is (= [1 2 3 4 5] (quicksort [1 2 3 5 4]))))
