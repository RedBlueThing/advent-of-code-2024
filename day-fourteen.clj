(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow sqrt]])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ["p=0,4 v=3,-3"
                    "p=6,3 v=-1,-3"
                    "p=10,3 v=-1,2"
                    "p=2,0 v=2,-1"
                    "p=0,0 v=1,3"
                    "p=3,0 v=-2,-2"
                    "p=7,6 v=-1,-3"
                    "p=3,0 v=-1,-2"
                    "p=9,3 v=2,3"
                    "p=7,3 v=-1,2"
                    "p=2,4 v=2,-3"
                    "p=9,5 v=-3,-3"])

(def real-data-raw (str/split-lines (slurp "day-fourteen.txt")))

;; rows and columns for the data sets
(def test-data-bathroom-space [7, 11])
(def real-data-bathroom-space [103, 101])

(defn new-wrapped-position [position velocity bounds]
  (let [unwrapped-position (+ position velocity)]
    (if (or (< unwrapped-position 0) (>= unwrapped-position bounds))
      (mod unwrapped-position bounds)
      unwrapped-position)))

(defn new-wrapped-x-and-y [bathroom-space [pos-x pos-y] [velocity-x velocity-y]]
  ;; return a robots new position based on the size of the bathroom (and the
  ;; potential to teleport at the walls), their current position and their
  ;; velocity.
  (let [[bathroom-rows bathroom-columns] bathroom-space
        new-pos-x (new-wrapped-position pos-x velocity-x bathroom-columns)
        new-pos-y (new-wrapped-position pos-y velocity-y bathroom-rows)]
    (assert (and (>= new-pos-x 0) (< new-pos-x bathroom-columns)))
    (assert (and (>= new-pos-y 0) (< new-pos-y bathroom-rows)))
    [new-pos-x new-pos-y]))

(assert (= (new-wrapped-x-and-y test-data-bathroom-space [2 4] [2 -3]) [4 1]))
(defn parse-data [data]
  (map (fn [line]
         (let [[pos-x pos-y velocity-x velocity-y]
               (map #(Integer/parseInt %)
                    (re-seq #"-?\d+" line))]
           [[pos-x pos-y] [velocity-x velocity-y]]))
       data))

(defn quadrant-for-position [[pos-x pos-y] [bathroom-rows bathroom-columns]]
  (let [middle-column (quot bathroom-columns 2)
        middle-row (quot bathroom-rows 2)]
    (cond
      (or (= pos-x middle-column) (= pos-y middle-row)) :center
      (and (< pos-x middle-column) (< pos-y middle-row)) :nw
      (and (< pos-x middle-column) (> pos-y middle-row)) :sw
      (and (> pos-x middle-column) (< pos-y middle-row)) :ne
      (and (< pos-x middle-column) (> pos-y middle-row)) :se)))

(defn quadrants [robots bathroom-space]
  ;; organise the robots by quadrant
  (reduce (fn [acc [quadrant [position velocity]]]
            (let [current (or (acc quadrant) [])]
              (if (= quadrant :center)
                acc
                (assoc acc quadrant (conj current [position velocity])))))

          {}
          (map (fn [[position velocity]]
                 [(quadrant-for-position position bathroom-space) [position velocity]])
               robots)))

(defn part-one [data bathroom-space]
  (let [robots (parse-data data)
        simulate-round-reducer (fn [acc round-index]
                                 (map
                                  (fn [[position velocity]]
                                    [(new-wrapped-x-and-y bathroom-space position velocity) velocity])
                                  acc))
        updated-robots (reduce simulate-round-reducer robots (range 0 100))
        robots-in-quadrants (quadrants updated-robots bathroom-space)]
    (apply * (map count (vals robots-in-quadrants)))))
