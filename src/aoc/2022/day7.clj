(ns aoc.2022.day7
  (:require
    [aoc.util :as util]
    [clojure.string :as s]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2022/day7-ex.txt")))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day7.txt")))

(defn exec-command [{:keys [pwd] :as m} [arg1 arg2 arg3]]
  (cond-> m
    (and (= "cd" arg2) (= ".." arg3) (not= ["/"] pwd)) (update-in [:pwd] pop)
    (and (= "cd" arg2) (not= ".." arg3))               (update-in [:pwd] conj arg3)
    (some? (parse-long arg1)) (update-in (concat [:system-tree] pwd) conj {arg2 (parse-long arg1)})
    (= "dir" arg1)            (update-in (concat [:system-tree] pwd) conj {arg2 {}})))

(comment
  (exec-command {:pwd ["/"] :system-tree {"/" {}}} ["dir" "a"])
  (exec-command {:pwd ["/"] :system-tree {"/" {"a" {"b" 123}}}} ["$" "cd" "a"])
  (exec-command {:pwd ["/"] :system-tree {"/" {"a" {"b" 123}}}} ["$" "cd" ".."]))

(defn parse-system-tree [input]
  (->> input
       (map #(s/split % #" "))
       (reduce exec-command {:pwd [] :system-tree {"/" {}}})))

(comment
  (parse-system-tree input)
  (parse-system-tree example))

(defn folders [system-tree]
  (->> (tree-seq map? #(interleave (keys %) (vals %)) system-tree)
       (filter map?) ; map as folder
       (drop 1))) ; drop root node

(defn folder-size [folder-tree]
  (->> (tree-seq map? #(vals %) folder-tree)
       (filter number?) ; number as size
       (apply +)))

(comment
  (folders {:/ {:a {:e {:i 584} :f 29116 :g 2557 :h.lst 62596}}})
  (folder-size {:i 584}))

(defn part1 [input]
  (let [system-tree (:system-tree (parse-system-tree input))
        folder-sizes (->> (folders system-tree)
                          (map folder-size))]
    (->> folder-sizes
         (filter #(<= % 100000))
         (apply +))))

(defn part2 [input]
  (let [system-tree (:system-tree (parse-system-tree input))
        folder-sizes (->> (folders system-tree)
                          (map folder-size))
        unused-space (- 70000000 (apply max folder-sizes))]
    (->> folder-sizes
         (filter #(<= 30000000 (+ unused-space %)))
         (apply min))))

(deftest test-example
  (is (= 95437 (part1 example)))
  (is (= 24933642 (part2 example))))

(comment
  (time (part1 input))  ; 1427048, "Elapsed time: 6.232834 msecs"
  (time (part2 input))) ; 2940614, "Elapsed time: 6.438917 msecs"
