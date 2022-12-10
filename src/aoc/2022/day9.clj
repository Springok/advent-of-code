(ns aoc.2022.day9
  (:require
    [aoc.util :as util]
    [clojure.string :as s]
    [clojure.test :refer [deftest is]]
    [clojure.math :as math]))

(def example
  (util/read-file-by-line "../resources/aoc/2022/day9-ex.txt"))

(def example2
  (util/read-file-by-line "../resources/aoc/2022/day9-ex2.txt"))

(def input
  (util/read-file-by-line "../resources/aoc/2022/day9.txt"))

(defn parse-commands [input]
  (->> input
       (map #(s/split % #" "))
       (map (fn [[command steps]] [command (read-string steps)]))
       (mapcat (fn [[cmd n]] (repeat n cmd)))))

(comment
  (parse-commands example))

(defn next-tails [[x y]]
  [[x y]
   [x (inc y)]
   [x (dec y)]
   [(inc x) y]
   [(dec x) y]
   [(inc x) (inc y)]
   [(inc x) (dec y)]
   [(dec x) (dec y)]
   [(dec x) (inc y)]])

(defn normal-step [[x y] dir]
 (case dir
   "U" [x (inc y)]
   "D" [x (dec y)]
   "R" [(inc x) y]
   "L" [(dec x) y]))

(defn step [[head-x head-y :as head] [tail-x tail-y :as tail] dir]
  (let [next-head (normal-step head dir)]
    (cond
      (some #(= next-head %) (next-tails tail)) [next-head tail] ; cover / touching
      (and (not= head-x tail-x) (not= head-y tail-y)) [next-head head] ; diagonal-follow, applicable to 2 knots only...
      :else [next-head (normal-step tail dir)])))

(defn next-state [state dir]
  (let [{:keys [head tail]} state
        [next-head next-tail] (step head tail dir)]
    (-> state
        (assoc :head next-head)
        (assoc :tail next-tail)
        (update :visited-tail conj next-tail))))

(defn part1 [input]
  (-> (reduce next-state {:head [0 0] :tail [0 0] :visited-tail #{}} (parse-commands input))
      (get :visited-tail)
      (count)))

; ===== Part 2 ====
; refer to other's approach on tail-step
; =================

(defn sub [a b] (mapv - a b))

(defn add [a b] (mapv + a b))

(defn direction [a b]
  (let [[dx dy] (sub a b)]
    [(long (math/signum dx)) (long (math/signum dy))]))

(defn tail-step [head tail]
  (if (some #(= head %) (next-tails tail))
    tail ; cover / touching
    (add tail (direction head tail))))

(defn knots-step [knots dir]
  (let [first-head (first knots)
        tails (drop 1 knots)
        next-head (normal-step first-head dir)]
     (reduce
       (fn [knots tail]
         (conj knots (tail-step (peek knots) tail)))
      [next-head]
      tails)))

(defn next-state-part2 [state dir]
  (let [knots (knots-step (:knots state) dir)]
    (-> state
        (assoc :knots knots)
        (update :visited-tail conj (last knots)))))

(defn part2 [input]
  (-> (reduce next-state-part2 {:knots (repeat 10 [0 0]) :visited-tail #{}} (parse-commands input))
      (get :visited-tail)
      (count)))

(deftest test-example
  (is (= 13 (part1 example)))
  (is (= 36 (part2 example2))))

(comment
  (time (part1 input))  ; => 6384, "Elapsed time: 17.967375 msecs"
  (time (part2 input))) ; => 2734, "Elapsed time: 124.812292 msecs"
