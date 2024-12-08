
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw [
"............"
"........0..."
".....0......"
".......0...."
"....0......."
"......A....."
"............"
"............"
"........A..."
".........A.."
"............"
"............"
])


(def test-data-raw-simple [".........."
                             ".........."
                             ".........."
                             "....a....."
                             ".........."
                             ".....a...."
                             ".........."
                             ".........."
                             ".........."
                             ".........."])

(def real-data-raw (str/split-lines (slurp "day-eight.txt")))

(defn combine-frequency-dictionaries [frequency-dictionaries]
  (reduce (fn [combined-dictionary locations] (merge-with into combined-dictionary locations)) {} frequency-dictionaries))

(defn parse-line [row line]
  (combine-frequency-dictionaries (map (fn [[k v]] {k v}) (filter some? (map-indexed (fn [column location-character]
                                                        (cond
                                                          (= location-character \.) nil
                                                          :else [location-character [[row column]]])) line)))))

(defn parse-data [data]
  ;; we want a dictionary mapping from the frequency character a list of rows
  ;; and columns
  (combine-frequency-dictionaries (map-indexed parse-line data)))


;; an antinode occurs at any point that is perfectly in line with two antennas of
;; the same frequency - but only when one of the antennas is twice as far away as
;; the other

(defn orientation-for-locations [[top-row top-column] [bottom-row bottom-column]]
  (cond
    (= top-row bottom-row) :e
    (= top-column bottom-column) :s
    (< top-column bottom-column) :se
    :else :ne
    )
  )

(defn is-point-in-range? [[row column] data]
  (and (>= row 0)
       (>= column 0)
       (< row (count data))
       (< column (count (first data)))))

(defn flatten-once [v] (into [] (mapcat identity) v))

(defn calculate-antinodes [data first-location second-location resonant-harmonics]
  (let [columns (count (first data))
        rows (count data)
        [top-location bottom-location] (sort (fn [[lr1 lc1] [lr2 lc2]] (<= lr1 lr2)) [first-location second-location])
        [top-row top-column] top-location
        [bottom-row bottom-column] bottom-location
        orientation (orientation-for-locations top-location bottom-location)]
    (let [row-distance-between (- bottom-row top-row)
          column-distance-between (Math/abs (- bottom-column top-column))
          resonant-range-left (fn [column-at-edge] (if resonant-harmonics (range 1 (inc (/ column-at-edge column-distance-between))) [1]))
          resonant-range-right (fn [column-at-edge] (if resonant-harmonics (range 1 (inc (/ (- columns column-at-edge) column-distance-between))) [1]))]

      (assert (not (or (= orientation :e) (= orientation :s))))

      (concat (if resonant-harmonics [first-location second-location] [])
              (case orientation
                :se (concat
                     (map (fn [mult] [(- top-row (* mult row-distance-between)) (- top-column (* mult column-distance-between))]) (resonant-range-left top-column))
                     (map (fn [mult] [(+ bottom-row (* mult row-distance-between)) (+ bottom-column (* mult column-distance-between))]) (resonant-range-right bottom-column)))

                :ne (concat
                     (map (fn [mult] [(- top-row (* mult row-distance-between)) (+ top-column (* mult column-distance-between))]) (resonant-range-right top-column))
                     (map (fn [mult] [(+ bottom-row (* mult row-distance-between)) (- bottom-column (* mult column-distance-between))]) (resonant-range-left bottom-column))))))))

(defn solve [data resonant-harmonics]
  (let [antenna-data (parse-data data)]
    (count (filter (fn [point] (is-point-in-range? point data))
                   (reduce (fn [antinode-set locations]
                             (into antinode-set (flatten-once
                                                 (map (fn [[first-location second-location]]
                                                        (calculate-antinodes data first-location second-location resonant-harmonics))
                                                      (combo/combinations locations 2))))) #{} (vals antenna-data))))))


(defn part-one [data]
  (solve data false))

(defn part-two [data]
  (solve data true))
