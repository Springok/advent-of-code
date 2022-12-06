(ns aoc.2022.day6
  (:require
    [aoc.util :as util]
    [clojure.test :refer [deftest is]]))

(def input
  (->> (util/read-file-by-line "../resources/aoc/2022/day6.txt")
       (first)))

(defn solve [chars window-size]
  (loop [window [0 window-size]
         packet {:maker window-size :processed-chars []}]
    (let [sub-chars (partial subvec chars)
          sub-chars' (apply sub-chars window)]
      (if (= (count sub-chars') (count (set sub-chars')))
          packet
          (recur (map inc window)
                 (-> packet
                     (assoc :maker (inc (last window)))
                     (update :processed-chars conj sub-chars')))))))

(defn part1 [input]
  (-> (solve (vec (char-array input)) 4)
      (get :maker)))

(defn part2 [input]
  (-> (solve (vec (char-array input)) 14)
      (get :maker)))

(deftest test-example
  (is (= 7 (part1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (is (= 5 (part1 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (is (= 6 (part1 "nppdvjthqldpwncqszvftbrmjlhg")))
  (is (= 10 (part1 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (is (= 11 (part1 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")))

  (is (= 19 (part2 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")))
  (is (= 23 (part2 "bvwbjplbgvbhsrlpgdmjqwftvncz")))
  (is (= 23 (part2 "nppdvjthqldpwncqszvftbrmjlhg")))
  (is (= 29 (part2 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")))
  (is (= 26 (part2 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))))

(comment
  (time (part1 input))   ; 1210 => "Elapsed time: 1.58375 msecs"
  (time (part2 input)))  ; 3476 => "Elapsed time: 6.37425 msecs"
