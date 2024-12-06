
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
      \# (let [new-direction (turn-right direction)
               [turn-row-index turn-column-index] (row-and-column-in-direction row-index column-index new-direction)]
           [turn-row-index turn-column-index new-direction]))))

(defn simulate-guard-movements [data]
  (let [[guard-start-location updated-data] (parse-data data)
        [guard-start-row-index guard-start-column-index] guard-start-location]
    (loop [guard-movements #{[guard-start-row-index guard-start-column-index]}
           last-movement [guard-start-row-index guard-start-column-index :up]]
      (let [[guard-row-index guard-column-index direction] last-movement
            next-guard-move (simulate-guard-movement updated-data guard-row-index guard-column-index direction)
            [new-row-index new-column-index direction] next-guard-move]
        (if (or (nil? next-guard-move) (= next-guard-move guard-start-location))
          (count guard-movements)
          (recur (conj guard-movements [new-row-index new-column-index]) next-guard-move))))))


