; [Day 15 - Advent of Code 2021](https://adventofcode.com/2021/day/15)
(ns aoc.2021.day15
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]
    [clojure.data.priority-map :refer [priority-map]]))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2021/day15-ex.txt")))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2021/day15.txt")))

(defn goal [board mag]
  (let [height (* mag (count board))
        width (* mag (count (first board)))]
    (mapv dec [height width])))

(defn neighbors [[y x] board mag]
  (for [[dy dx] [[1 0] [-1 0] [0 1] [0 -1]]
        :let [y' (+ y dy)
              x' (+ x dx)]
        :when (and (< -1 y' (* mag (count board)))
                   (< -1 x' (* mag (count (first board)))))]
    [y' x']))

(defn board [input]
  (let [board (->> input
                   (mapv (fn [line] (mapv #(Integer/parseInt (str %)) line))))]
    board))

(defn risk [coord board]
   (get-in board coord))

(defn risk-2 [[y x] board]
  ; 9 -> 1, 10 -> 2, 11 -> 3
  (let [ori-h (count board)
        ori-w (count (first board))
        ori-cost (get-in board [(mod y ori-h) (mod x ori-w)])]
   (->
     (+ ori-cost
        (quot y ori-h)
        (quot x ori-w))
     dec
     (mod 9)
     (inc))))


(defn initial-state [start]
  {:visited #{start}
   :frontier (priority-map {:coord start :cost 0} 0)})

(defn dijkstra [{:keys [board start goal mag]}]
  (loop [state (initial-state start)
         idx 0]
    (let [{:keys [visited frontier]} state
          [{:keys [coord cost]}] (peek frontier)]
      (if (= coord goal)
        [coord cost]
        (recur {:visited (conj visited coord)
                :frontier (into (pop frontier)
                                (for [coord' (neighbors coord board mag)
                                      :when (not (visited coord'))
                                      :let [cost' (+ cost (risk-2 coord' board))]]
                                  [{:coord coord' :cost cost'} cost']))}
               (inc idx))))))

 ; inputs: map(board), start [0, 0], goal
 ; output: lowest risk
 ; keep track of the current cost
 ; keep track of visited node
 ; priority queue for unvisited node

(defn part1 [input]
  (let [board (board input)
        goal (goal board 1)]
     (dijkstra {:board board
                :mag 1
                :start [0 0]
                :goal goal})))

(defn part2 [input]
  (let [board (board input)
        goal (goal board 5)]
     (dijkstra {:board board
                :mag 5
                :start [0 0]
                :goal goal})))

(comment
 (into (pop (priority-map [0 1] 7 [0 0] 6)) [[[0 7] 1] [[12 23] 2]])

 (-> (board example)
     (goal 5))

 (get-in (board example) [1 1])

 (neighbors [1 1] (board example) 1)

 (dijkstra {:board (board example)
            :mag 1
            :start [0 0]
            :goal (goal (board example) 1)}))


(deftest test-example
  (is (= 40 (second (part1 example))))
  (is (= 315 (second (part2 example)))))

(deftest test-input
  (is (= 613 (second (part1 input))))
  (is (= 2899 (second (part2 input)))))

