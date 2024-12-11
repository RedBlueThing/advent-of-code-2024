(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.math.combinatorics :as combo])


(def test-data-raw-one ["0 1 10 99 999"])

(def expected-test-one-blink-once [ 1 2024 1 0 9 9 2021976 ])

(def test-data-raw-two ["125 17"])

(def expected-test-two-blink-six-tgimes [ 2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2 ])

(def real-data-raw (str/split-lines (slurp "day-eleven.txt")))

(defn parse-data [data]
  (map #(Integer/parseInt %) (str/split (first data) #" ")))

(defn core-apply-rules [stone-number]

  (let [stone-str (str stone-number)]
    (cond
      ;; 1. If the stone is engraved with the number 0, it is replaced by a stone
      ;; engraved with the number 1.
      (= stone-number 0) [1]

      ;; 2. If the stone is engraved with a number that has an even number of digits,
      ;; it is replaced by two stones. The left half of the digits are engraved on the
      ;; new left stone, and the right half of the digits are engraved on the new
      ;; right stone. (The new numbers don't keep extra leading zeroes: 1000 would
      ;; become stones 10 and 0.)
      (= (mod (count stone-str) 2) 0) (let [i (/ (count stone-str) 2)] [(Integer/parseInt (subs stone-str 0 i)) (Integer/parseInt (subs stone-str i))])

      ;; 3. If none of the other rules apply, the stone is replaced by a new stone;
      ;; the old stone's number multiplied by 2024 is engraved on the new stone.
      :else [(* stone-number 2024)]

    )))

(def apply-rules (memoize core-apply-rules))

(defn core-apply-rules-to-stones-reducer [stones index]
  (println index)
  (apply concat (map apply-rules stones)))

(def apply-rules-to-stones-reducer (memoize core-apply-rules-to-stones-reducer))

(defn apply-rules-to-stones [stones blinks]
  (reduce apply-rules-to-stones-reducer stones (range 0 blinks)))

(defn part-one [data]
  (count (apply-rules-to-stones (parse-data data) 25)))

(defn part-two [data]
  (count (apply-rules-to-stones (parse-data data) 75)))