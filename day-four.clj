
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["MMMSXXMASM"
                    "MSAMXMSMSA"
                    "AMXSXMAAMM"
                    "MSAMASMSMX"
                    "XMASAMXAMM"
                    "XXAMMXXAMA"
                    "SMSMSASXSS"
                    "SAXAMASAAA"
                    "MAMMMXMMMM"
                    "MXMXAXMASX"])

(def real-data-raw (str/split-lines (slurp "day-four.txt")))

(def expected-test-xmas-count 18)

(defn parse-data [data]
  (map (fn characters-for-line [line] (map char line)) data))

;; check for words in these cardinal directions from an origin point
(def directions [:n :ne :e :se :s :sw :w :nw])

(defn character-at-row-column [row column word-search-grid]
  (let [rows (count word-search-grid)
        columns (count (first word-search-grid))]
    (if (or (< row 0)
            (< column 0)
            (>= row rows)
            (>= column columns))
      ;; if the location is out of range, just return nil (which won't match our character)
      nil
      ;; otherwise, what is the character at the location?
      (nth (nth word-search-grid row) column))))


(def row-offset-in-direction {
                              :n -1
                              :ne -1
                              :e 0
                              :se 1
                              :s 1
                              :sw 1
                              :w 0
                              :nw -1})

(def column-offset-in-direction {
                                 :n 0
                                 :ne 1
                                 :e 1
                                 :se 1
                                 :s 0
                                 :sw -1
                                 :w -1
                                 :nw -1
                                 })

(defn new-row-offset-in-direction [row direction offset]
  (+ row (* (row-offset-in-direction direction) offset)))

(defn new-column-offset-in-direction [column direction offset]
  (+ column (* (column-offset-in-direction direction) offset)))

(defn character-offset-from-row-column-in-direction [row column word-search-grid direction offset]
  ;; what character sits offset steps from the row column provided in the direction provided? (nil if out of range)
  (character-at-row-column (new-row-offset-in-direction row direction offset)
                           (new-column-offset-in-direction column direction offset)
                           word-search-grid))

(defn matches-word? [word row column word-search-grid direction]
  ;; predicate to check if the characters defined by a starting row and column,
  ;; in the provided direction match the word provided
  (every? identity (map-indexed (fn [idx word-character] (= word-character (character-offset-from-row-column-in-direction row column word-search-grid direction idx))) word)))

(defn count-words-from-location [word row column word-search-grid]
  (let [rows (count word-search-grid)
        columns (count (first word-search-grid))]
    (count (filter true? (map (fn [direction] (matches-word? word row column word-search-grid direction)) directions)))))

(defn part-one [data]
  (let [word-search-grid (parse-data data)
        rows (count word-search-grid)
        columns (count (first word-search-grid))]
    (reduce + (map (fn [column] (reduce + (map (fn [row] (count-words-from-location "XMAS" row column word-search-grid)) (range 0 columns)))) (range 0 rows)))))

(assert (= (part-one test-data-raw) expected-test-xmas-count))

(def expected-test-x-mas-count 9)

(defn is-x-mas-at-location? [row column word-search-grid]
  (if (= (character-at-row-column row column word-search-grid) \A)
    (and (or (matches-word? "MAS" (new-row-offset-in-direction row :nw 1) (new-column-offset-in-direction column :nw 1) word-search-grid :se)
             (matches-word? "MAS" (new-row-offset-in-direction row :se 1) (new-column-offset-in-direction column :se 1) word-search-grid :nw))
         (or (matches-word? "MAS" (new-row-offset-in-direction row :ne 1) (new-column-offset-in-direction column :ne 1) word-search-grid :sw)
             (matches-word? "MAS" (new-row-offset-in-direction row :sw 1) (new-column-offset-in-direction column :sw 1) word-search-grid :ne)))
    ;; if it's not an \A in the middle, then it can't be an x-mas cross
    false))

(defn part-two [data]
  (let [word-search-grid (parse-data data)
        rows (count word-search-grid)
        columns (count (first word-search-grid))]
    (reduce + (map (fn [column] (reduce + (map (fn [row] (if (is-x-mas-at-location? row column word-search-grid) 1 0)) (range 0 columns)))) (range 0 rows)))))

(assert (= (part-two test-data-raw) expected-test-x-mas-count))
