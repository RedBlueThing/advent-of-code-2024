
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw-small ["0123"
                          "1234"
                          "8765"
                          "9876"])


(def test-data-raw-medium ["89010123"
                           "78121874"
                           "87430965"
                           "96549874"
                           "45678903"
                           "32019012"
                           "01329801"
                           "10456732"])

(def real-data-raw (str/split-lines (slurp "day-ten.txt")))

;; a hiking trail is any path that starts at height 0, ends at height 9, and always increases by a height of exactly 1 at each step

(defn parse-data [data]
  (map (fn [row] (map #(Integer/parseInt %) (map str row))) data))

(defn row-and-column-in-direction [row column direction]
  (case direction
    :n [(dec row) column]
    :e [row (inc column)]
    :s [(inc row) column]
    :w [row (dec column)]))

(defn in-range? [data [row column]]
  (let [rows (count data)
        columns (count (first data))]
    (and (>= row 0)
         (< row rows)
         (>= column 0)
         (< column columns))))

(defn height-at-row-column [data [row column]]
  (nth (nth data row) column))

(defn viable-next-steps [data [row column]]
  ;; given a location on the heightmap are their any paths from this location?
  ;; What are their coordinates?
  (let [current-height (height-at-row-column data [row column])]
    (filter (fn path-predicate[[target-row target-column]]
              ;; paths increase by one step at a time
              (= (height-at-row-column data [target-row target-column]) (inc current-height)))
          (filter #(in-range? data %) (map (fn [direction] (row-and-column-in-direction row column direction)) [:n :e :s :w])))))

(assert (= (viable-next-steps (parse-data test-data-raw-small) [0 0]) [[0 1][1 0]]))

(defn flatten-once [v] (into [] (mapcat identity) v))

(defn trailheads [data]
  ;; return the coordinates of all the trailheads
  (filter some?
          (flatten-once (let [rows (count data)
                columns (count (first data))]
            (map (fn [row]
                   (map (fn [column]
                          (if (= (height-at-row-column data [row column]) 0)
                            [row column]
                            nil))
                        (range 0 columns)))
         (range 0 rows))))))


(assert (= (trailheads (parse-data test-data-raw-small)) [ [0 0] ]))
(assert (= (trailheads (parse-data test-data-raw-medium)) [ [0 2] [0 4] [2 4] [4 6] [5 2] [5 5] [6 0] [6 6] [7 1]]))

(defn find-hiking-trails [data [row column] _]
  ;; return a distinct set of endpoints for valid hiking trails

  ;; is this an endpoint?
  (if (= (height-at-row-column data [row column]) 9)
    #{[row column]}
    ;; otherwise keep looking
    (let [next-steps (viable-next-steps data [row column])]
      (reduce set/union (map
                         (fn [[row column]] (find-hiking-trails data [row column] _))
                         next-steps)))))


(defn find-distinct-hiking-trails [data [row column] distinct-trails]
  ;; return a distinct set of endpoints for valid hiking trails

  ;; is this an endpoint?
  (let [updated-distinct-trails (map #(conj % [row column]) distinct-trails)]
    (if (= (height-at-row-column data [row column]) 9)
      (set updated-distinct-trails)
      ;; otherwise keep looking
      (let [next-steps (viable-next-steps data [row column])]
        (reduce set/union (map
                         (fn [[row column]] (find-distinct-hiking-trails data [row column] updated-distinct-trails))
                         next-steps))))))

(defn score-trailhead [data [row column] find-trails-fn]
  (count (find-trails-fn data [row column] [[]])))

(defn part-one [data]
  (reduce + (map (fn [[row column]] (score-trailhead data [row column] find-hiking-trails)) (trailheads data))))

(defn part-two [data]
  (reduce + (map (fn [[row column]] (score-trailhead data [row column] find-distinct-hiking-trails)) (trailheads data))))
