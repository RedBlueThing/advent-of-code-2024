(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.math.combinatorics :as combo])


(def test-data-raw-small ["AAAA"
                          "BBCD"
                          "BBCC"
                          "EEEC"])


(def test-data-raw-medium ["OOOOO"
                           "OXOXO"
                           "OOOOO"
                           "OXOXO"
                           "OOOOO"])

(def test-data-raw-mobius ["AAAAAA"
                           "AAABBA"
                           "AAABBA"
                           "ABBAAA"
                           "ABBAAA"
                           "AAAAAA"])

(def test-data-raw-large ["RRRRIICCFF"
                          "RRRRIICCCF"
                          "VVRRRCCFFF"
                          "VVRCCCJFFF"
                          "VVVVCJJCFE"
                          "VVIVCCJJEE"
                          "VVIIICJJEE"
                          "MIIIIIJJEE"
                          "MIIISIJEEE"
                          "MMMISSJEEE"])

(def real-data-raw (str/split-lines (slurp "day-twelve.txt")))

(defn row-and-column-in-direction [row column direction]
  (case direction
    :n [(dec row) column]
    :e [row (inc column)]
    :s [(inc row) column]
    :w [row (dec column)]
    ;; for part two we need diagonals as well
    :ne [(dec row) (inc column)]
    :se [(inc row) (inc column)]
    :sw [(inc row) (dec column)]
    :nw [(dec row) (dec column)]))

(defn in-range? [data [row column]]
  (let [rows (count data)
        columns (count (first data))]
    (and (>= row 0)
         (< row rows)
         (>= column 0)
         (< column columns))))

(defn parse-data [data]
  (map (fn [row] (map char row)) data))

(defn plant-type-at-row-column [data [row column]]
  (nth (nth data row) column))

(defn neighbours-for-location [[row column]]
  (map
   (fn [direction] (row-and-column-in-direction row column direction))
   [:n :e :s :w]))

(defn core-neighbours-in-region [data current-region-set location]
  ;; neighbours of the location that aren't already in the region set
  (let [[row column] location
        current-plant-type (plant-type-at-row-column data [row column])
        neighbours-in-range (filter (fn [location] (in-range? data location)) (neighbours-for-location [row column]))
        neighbours-not-already-in-set (filter (fn [location] (not (current-region-set location))) neighbours-in-range)]
    (set (filter (fn [location] (= (plant-type-at-row-column data location) current-plant-type)) neighbours-not-already-in-set))))

(def neighbours-in-region (memoize core-neighbours-in-region))

(defn build-region-set [current-region-set data [row column]]
  ;; recurse from a location building up a set of locations in the same region.
  (let [neighbours (neighbours-in-region data @current-region-set [row column])
        updated-region-set (swap! current-region-set set/union neighbours @current-region-set #{[row column]})]
    (if (empty? neighbours)
      ;; return the current region
      updated-region-set
      ;; keep looking
      (apply set/union (map (fn [neighbour] (build-region-set current-region-set data neighbour)) neighbours)))))

(defn build-region-sets [data]
  ;; build a set of locations in each distinct region

  (let [rows (count data)
        columns (count (first data))
        rows-and-columns (for [row (range 0 rows) column (range 0 columns)] [row column])]

    ((reduce
      (fn [acc location]
        (let [{found-location-set :found-location-set region-sets :region-sets} acc]
          (if (found-location-set location)
            acc
           ;; if we haven't found this location yet, we can build a region set for it
            (let [new-region-set (build-region-set (atom #{}) data location)]
              {:region-sets (conj region-sets new-region-set) :found-location-set (reduce conj found-location-set new-region-set)}))))

      {:found-location-set #{} :region-sets []} rows-and-columns) :region-sets)))

(defn index-for-row-column [data row column]
  (let [rows (count data)
        columns (count (first data))]
    (+ (* row columns) column)))

(defn ordered-region-set [data region-set]
  (sort
   (fn [[row1 column1] [row2 column2]]
     (let [index1 (index-for-row-column (parse-data test-data-raw-large) row1 column1)
           index2 (index-for-row-column (parse-data test-data-raw-large) row2 column2)]
       (< index1 index2)))
   (vec region-set)))

(defn ordered-region-sets [data region-sets]
  (map (fn [region-set] (ordered-region-set data region-set)) region-sets))

(defn perimeter-for-location [data location]
  (- 4 (count (neighbours-in-region data #{} location))))

(defn price-for-region-set [data region-set]
  (* (count region-set) (reduce + (map #(perimeter-for-location data %) region-set))))

(defn total-price [data]
  (reduce + (map #(price-for-region-set data %) (build-region-sets data))))

(defn touching-for-location [[row column]]
  (into {} (map
    (fn [direction] [ direction (row-and-column-in-direction row column direction) ])
    [:nw :n :ne :w :e :sw :s :se])))

(defn match-profile-around-location [data [row column]]
  (let [current-plant-type (plant-type-at-row-column data [row column])
        touching-locations (touching-for-location [row column])]
    (into {} (map (fn [key] [key (let [touching-location (touching-locations key)]
                                   (if (in-range? data touching-location)
                                     (= (plant-type-at-row-column data touching-location) current-plant-type)
                                     ;; if it's not in range then  we just treat it like an out of region location
                                     false))]) (keys touching-locations)))))

;; this dictionary maps from a vector left square, right square and straight
;; ahead square (in that order) to a flag indicating if there is a corner in
;; that direction.
(def is-corner {[false false true] true
                [true false false] false
                [false true false] false
                [false false false] true
                [true true true] false
                [true true false] true
                [false true true] false
                [true false true] false})

(def left-right-ahead-for-directions {
                                       ;; maps from the direction we are
                                       ;; checking for a corner to the squares
                                       ;; left, rigth and ahead (for is-corner)
                                       ;; relative to the square we are checking
                                       :nw [:w :n :nw]
                                       :sw [:s :w :sw]
                                       :se [:e :s :se]
                                       :ne [:n :e :ne]})

(defn corners-for-location [data [row column]]
  (let [profile (match-profile-around-location data [row column])]
    (count (filter true? (map (fn [direction]
                                (is-corner (map-indexed (fn [i relative-direction]
                                                          (profile relative-direction))
                                                        (left-right-ahead-for-directions direction))))
                              [:nw :sw :se :ne])))))

(defn corners-for-region-set [data region-set]
  (reduce + (map #(corners-for-location data %) region-set)))

(defn discount-price-for-region-set [data region-set]
  (* (count region-set) (corners-for-region-set data region-set)))

(defn total-price-with-discount [data]
  (reduce + (map #(discount-price-for-region-set data %) (build-region-sets data))))
