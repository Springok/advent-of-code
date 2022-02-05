; [Day 2 - Advent of Code 2021](https://adventofcode.com/2021/day/2)
(ns aoc.2021.day2
  (:require
    [aoc.util :as util]
    [clojure.string :as string]
    [clojure.test :refer [deftest is]]))

(defn updated-map [m]
  (reduce-kv (fn [m k v]
              (assoc m (keyword k) (Integer/parseInt v))) {} m))

(comment
  (updated-map {"forward" "2"}))

(def example
  [{:forward 5}
   {:down 5}
   {:forward 8}
   {:up 3}
   {:down 8}
   {:forward 2}])


(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day2.txt")
       (map #(apply array-map (string/split % #" ")))
       (map updated-map)))

(defn dive
  [pos {:keys [forward down up] :or {forward 0 down 0 up 0} :as move}]
  (prn (str "previous position: " pos ", action: " move))
  (-> pos
      (update-in [:horizontal] #(+ % forward))
      (update-in [:depth] #(+ % down))
      (update-in [:depth] #(- % up))))

(defn dive-in-aim
  [{:keys [aim] :as pos} {:keys [forward down up] :or {forward 0 down 0 up 0} :as move}]
  (prn (str "previous position: " pos ", action: " move))
  (-> pos
      (update-in [:horizontal] #(+ % forward))
      (update-in [:depth] #(+ % (* forward aim)))
      (update-in [:aim]   #(+ % down))
      (update-in [:aim]   #(- % up))))

(defn dest
  ([steps move-fn] (dest {:horizontal 0 :depth 0 :aim 0} steps move-fn))
  ([from steps move-fn]
   (reduce move-fn from steps)))

(defn hxd [{:keys [horizontal depth aim]}]
  (prn (str "destination: " "horizontal: " horizontal ", depth: " depth ", aim: " aim))
  (* horizontal depth))

(defn part1 [input]
  (-> (dest input dive)
      (hxd)))

(defn part2 [input]
  (-> (dest input dive-in-aim)
      (hxd)))

(comment
  ;; part1 => 2215080, horizontal: 1890, depth: 1172
  (part1 input)

  ;; part2 => 1864715580,  destination: horizontal: 1890, depth: 986622, aim: 1172
  (part2 input))


(deftest test-example
   (let [sample example]
     (is (= 150 (part1 sample)))
     (is (= 900 (part2 sample)))))
