(ns aoc.2022.day9
  (:require
    [aoc.util :as util]
    [clojure.string :as s]
    [clojure.test :refer [deftest is]]))

(def example
  (->> (util/read-file-by-line "../resources/aoc/2022/day9-ex.txt")
       (map #(s/split % #" "))
       (map (fn [[command steps]] [command (read-string steps)]))))

(def example-2
  (->> (util/read-file-by-line "../resources/aoc/2022/day9-ex2.txt")
       (map #(s/split % #" "))
       (map (fn [[command steps]] [command (read-string steps)]))))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day9.txt")
       (map #(s/split % #" "))
       (map (fn [[command steps]] [command (read-string steps)]))))

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
      ; (= head tail) [next-head tail] ; cover
      (some #(= next-head %) (next-tails tail)) [next-head tail] ; cover / touching
      (and (not= head-x tail-x) (not= head-y tail-y)) [next-head head] ; diagonal-follow
      :else [next-head (normal-step tail dir)])))

(defn next-state [init-state [dir steps]]
  (loop [remaining-steps steps
         state init-state]
    (if (= 0 remaining-steps)
      state
      (let [{:keys [head tail]} state
            [next-head next-tail] (step head tail dir)]
        (recur (dec remaining-steps)
               (-> state
                   (assoc :head next-head)
                   (assoc :tail next-tail)
                   (update :visited-tail conj next-tail)))))))

(comment
  (next-state {:head [0 0] :tail [0 0] :visited-tail #{}} ["R" 2])
  (-> (next-state (next-state {:head [0 0] :tail [0 0] :visited-tail #{}} ["R" 4]) ["U" 4])
      (get :visited-tail)
      (count)))

(defn part1 [input]
  (let [initial-state {:head [0 0] :tail [0 0] :visited-tail #{}}]
    (-> (reduce next-state initial-state input)
        (get :visited-tail)
        (count))))

(defn tail-step [[head-x head-y :as head] [tail-x tail-y :as tail] dir]
  (let [next-head (normal-step head dir)]
    (cond
      ; (= head tail) [next-head tail] ; cover
      (some #(= next-head %) (next-tails tail)) tail ; cover / touching
      (and (not= head-x tail-x) (not= head-y tail-y)) head ; diagonal-follow
      :else (normal-step tail dir))))

(defn knots-step [knots dir]
  (let [[first-head first-tail] (take 2 knots)
         next-head (normal-step first-head dir)
         next-first-tail (tail-step first-head first-tail dir)
         pairs (partition 2 1 (concat [next-first-tail] (drop 1 knots)))]
     (reduce
       (fn [knots [head tail]]
         (conj knots (tail-step head tail dir)))
      [next-head]
      pairs)))
  ; [1,0] [0,0]....

(comment
  (let [knots (->> (repeat 5 "R")
                   (reduce #(knots-step %1 %2) (repeat 10 [0 0])))]
     (prn knots)
     (->> (repeat 2 "U")
          (reduce #(knots-step %1 %2) knots))))

; (defn step-2 [[head-x head-y :as head] tails dir]
;   (let [next-head (normal-step head dir)]
;     (cond
;       ; (= head tail) [next-head tail] ; cover
;       (some #(= next-head %) (next-tails tail)) [next-head tail] ; cover / touching
;       (and (not= head-x tail-x) (not= head-y tail-y)) [next-head head] ; diagonal-follow
;       :else [next-head (normal-step tail dir)])))

(defn next-state-2 [init-state [dir steps]]
  (loop [remaining-steps steps
         state init-state]
    (if (= 0 remaining-steps)
      state
      (let [{:keys [head tail]} state
            [next-head next-tail] (step head tail dir)]
        (recur (dec remaining-steps)
               (-> state
                   (assoc :head next-head)
                   (assoc :tail next-tail)
                   (update :visited-tail conj next-tail)))))))

(defn part2 [input]
  (let [initial-state {:head [0 0] :tails (repeat 9 [0 0]) :visited-tail #{}}]
    (-> (reduce next-state initial-state input))))

(deftest test-example
  (is (= 13 (part1 example))))
  ; (is (= 13 (part2 example))))
(comment
  (first (first (partition 2 1 (take 2 (repeat 10 [0 0])))))
  (count (identity input))
  (time (part1 input))  ; => 6384, "Elapsed time: 17.967375 msecs"
  (time (part2 input))) ;
