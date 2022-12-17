(ns aoc.2022.day12
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]
    [clojure.data.priority-map :refer [priority-map]]))

(def example
  (util/read-file-by-line "../resources/aoc/2022/day12-ex.txt"))

(def input
  (util/read-file-by-line "../resources/aoc/2022/day12.txt"))

(defn get-h [grid [r c]]
  (-> grid
      (nth c)
      (nth r)))

(defn parse-map [input]
  (map char-array input))

(defn can-visit? [e1 e2]
  (let [order (zipmap (concat [\S] (map char (range 97 (inc 122))) [\E])
                      (concat [1] (range 1 (inc 26)) [26]))]
    (>= 1 (- (get order e2)
             (get order e1)))))

(comment
  (can-visit? \y \E))

(defn neighbors [[r c] h-map]
  (for [[dr dc] [[-1 0] [1 0] [0 1] [0 -1]]
        :let [r' (+ r dr)
              c' (+ c dc)]
        :when (and (< -1 r' (count (first h-map)))
                   (< -1 c' (count h-map))
                   (can-visit? (get-h h-map [r c]) (get-h h-map [r' c'])))]
    [r' c']))

(defn starts [input]
   (->> (let [h-map (parse-map input)
              r-count (count (first input))
              c-count (count input)]
          (for [r (range r-count)
                c (range c-count)]
            {[r c] (get-h h-map [r c])}))
        (apply conj)
        (reduce-kv (fn [a k v]
                     (case v
                       \S (-> a
                            (assoc :start k)
                            (update :starts conj k))
                       \a (update a :starts conj k)
                       a))
                   {:starts []})))

(defn dijkstra [{:keys [h-map start goal]}]
  (loop [state {:visited #{start}
                :queue (priority-map start 0)}
         idx 0]
    (let [{:keys [visited queue]} state
          [coord cost] (peek queue)
          current-elevation (get-h h-map coord)]
      (if (= current-elevation goal)
        [coord cost]
        (recur {:visited (conj visited coord)
                :queue (into (pop queue)
                             (for [coord' (neighbors coord h-map)
                                   :when (not (visited coord'))]
                               [coord' (inc cost)]))}
               (inc idx))))))

(defn part1 [input]
  (->> (dijkstra {:h-map (parse-map input)
                  :start (:start (starts input))
                  :goal \E})
       (last)))

(defn part2 [input]
  (->> (:starts (starts input))
       (filter (fn [[r _c]] (= r 0)))
       (map #(dijkstra {:h-map (parse-map input)
                               :start %
                               :goal \E}))
       (map last)
       (apply min)))

(deftest test-example
  (is (= 31 (part1 example)))
  (is (= 29 (part2 example))))

(defn -main [& _]
  (println "part 1:" (time (part1 input)))
  (println "part 2:" (time (part2 input))))

(comment
  (-main))
