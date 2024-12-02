
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

;; part one
;; --------
;; The levels are either all increasing or all decreasing.
;; Any two adjacent levels differ by at least one and at most three.

(def test-data-raw ["7 6 4 2 1"
                    "1 2 7 8 9"
                    "9 7 6 2 1"
                    "1 3 2 4 5"
                    "8 6 4 4 1"
                    "1 3 6 7 9"])

(def real-data-raw (str/split-lines (slurp "day-two.txt")))

(def expected-part-one-test-result 11)

(defn parse-reports [reports]
  (map (fn parse-report [report-line] (map #(Integer/parseInt %) (str/split report-line #" "))) reports))

(defn progression-for-report [report]
  (filter some? (map-indexed (fn [idx current-level] (if (< idx (dec (count report)))
                                                      (let [next-level (nth report (inc idx))]
                                                        [(Math/abs (- current-level next-level))
                                                         (if (< current-level next-level) :increasing :decreasing)])
                                                      nil)) report)))

(defn safe-report? [report]
  (let [progression (progression-for-report report)
        first-direction (second (first progression))]
    (every? (fn [[difference direction]]
              (and (= direction first-direction)
                   (>= difference 1)
                   (<= difference 3))) progression)))

(defn part-one [data]
  (let [reports (parse-reports data)]
    (count (filter safe-report? reports))))


(defn splice-out [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(defn all-variants [report]
  ;; returns all variants of a report by removing a single level
  (map (fn [index] (splice-out (vec report) index)) (range 0 (count report))))

(defn safe-report-with-problem-dampener? [report]
  (if (safe-report? report)
    true
    ;; otherwise try removing levels to get a safe report
    (some safe-report? (all-variants report))
    ))

(defn part-two [data]
  (let [reports (parse-reports data)]
    (count (filter safe-report-with-problem-dampener? reports))))
