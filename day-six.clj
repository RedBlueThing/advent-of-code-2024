
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["....#....."
                    ".........#"
                    ".........."
                    "..#......."
                    ".......#.."
                    ".........."
                    ".#..^....."
                    "........#."
                    "#........."
                    "......#..."])

(def real-data-raw (str/split-lines (slurp "day-six.txt")))

(defn parse-data [data]
  (let [[guard-row-index guard-column-index] (first (filter some? (map-indexed (fn [row-index line] (let [guard-index (str/last-index-of line "^")]
                                                                                                      (if guard-index
                                                                                                        [row-index guard-index]
                                                                                                        nil))) data)))
        ;; remove the guard caret from the data
        updated-data (map (fn [line] (str/replace line #"\^" ".")) data)]
    [[guard-row-index guard-column-index] updated-data]))

(def directions [:up :right :down :left])

(defn row-and-column-in-direction [row-index column-index direction]
  (case direction
    :up [(dec row-index) column-index]
    :right [row-index (inc column-index)]
    :down [(inc row-index) column-index]
    :left [row-index (dec column-index)]
    :else (do
            (assert false)
            [-1 -1])))

(defn turn-right [direction]
  ({:up :right
    :right :down
    :down :left
    :left :up
    } direction)
  )

(defn row-and-column-out-of-range [data new-row-index new-column-index]
  (or (< new-row-index 0)
      (< new-column-index 0)
      (>= new-row-index (count data))
      (>= new-column-index (count (first data)))))

(defn simulate-guard-movement [data row-index column-index direction]
  (let [[new-row-index new-column-index] (row-and-column-in-direction row-index column-index direction)
        out-of-range (row-and-column-out-of-range data new-row-index new-column-index)
        value-in-direction (if out-of-range
                             nil
                             (nth (nth data new-row-index) new-column-index))]
    (case value-in-direction
      ;; fine to keep going
      \. [new-row-index new-column-index direction]
      ;; outside the map, we are done
      nil nil
      ;; an obstacle, turn right
      \# (let [new-direction (turn-right direction)] [row-index column-index new-direction]))))

(defn simulate-guard-movements [data]
  (let [[guard-start-location updated-data] (parse-data data)
        guard-start-movement (conj guard-start-location :up)
        [guard-start-row-id guard-start-column-id] guard-start-location]
    (loop [guard-movements #{guard-start-location}
           last-movement guard-start-movement
           loop-count 0
           visited-move-set #{}]
      (let [[guard-row-index guard-column-index direction] last-movement
            next-guard-move (simulate-guard-movement updated-data guard-row-index guard-column-index direction)
            [new-row-index new-column-index direction] next-guard-move
            new-loop-count (if (contains? visited-move-set next-guard-move) (inc loop-count) loop-count)
            looping (> new-loop-count 1)]
        (if (or (nil? next-guard-move) looping)
          [(count guard-movements) looping]
          (recur (conj guard-movements [new-row-index new-column-index]) next-guard-move new-loop-count (conj visited-move-set next-guard-move)))))))

(defn insert-obstacle-at-row-column [data row column]
  (map-indexed (fn [idx line] (if (= idx row)
                                (str (subs line 0 column) "#" (subs line (inc column)))
                                line)) data))

(defn part-one [data] (first (simulate-guard-movements data)))

(defn part-two [data]
  (let [rows (count data)
        columns (count (first data))]
    (count (filter true? (map (fn [[obstacle-row obstacle-column]]
                                (println obstacle-row)
                                (if (= (nth (nth data obstacle-row) obstacle-column) \.)
                                  (let [situation (insert-obstacle-at-row-column data obstacle-row obstacle-column)
                                        [_ looping] (simulate-guard-movements situation)]
                                    looping)
                                  ;; if there is already an obstacle at the location, then no need to test it (it doesn't loop)
                                  false))
                              (for [row (range 0 rows)
                                    column (range 0 columns)] [row column]))))))
