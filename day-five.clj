
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["47|53"
                    "97|13"
                    "97|61"
                    "97|47"
                    "75|29"
                    "61|13"
                    "75|53"
                    "29|13"
                    "97|29"
                    "53|29"
                    "61|53"
                    "97|53"
                    "61|29"
                    "47|13"
                    "75|47"
                    "97|75"
                    "47|61"
                    "75|61"
                    "47|29"
                    "75|13"
                    "53|13"
                    ""
                    "75,47,61,53,29"
                    "97,61,53,29,13"
                    "75,29,13"
                    "75,97,47,61,53"
                    "61,13,29"
                    "97,13,75,29,47"])

(def real-data-raw (str/split-lines (slurp "day-five.txt")))

(defn parse-data [data]
  (let [partitions (partition-by #(= "" %) data)
        page-ordering-rules (map (fn [line] (map #(Integer/parseInt %) (str/split line #"\|"))) (first partitions))
        updates (map (fn [line] (map #(Integer/parseInt %) (str/split line  #","))) (last partitions))]
    [page-ordering-rules updates]))

(defn index-of [value coll]
  (some (fn [[idx sequence-val]] (when (= sequence-val value) idx)) (map-indexed vector coll)))

(defn valid-update-for-rule? [current-update rule]
  (let [[first-page second-page] rule
        maybe-first-page-index (index-of first-page current-update)
        maybe-second-page-index (index-of second-page current-update)]
    (if (and maybe-first-page-index maybe-second-page-index)
      (< maybe-first-page-index maybe-second-page-index)
      ;; if the rule pages aren't both in this update, then we assume it's valid
      true
      )))

(defn valid-update? [current-update page-ordering-rules]
  (every? true? (map (fn [rule] (valid-update-for-rule? current-update rule)) page-ordering-rules)))

(defn part-one [data]
  ;; Determine which updates are already in the correct order. What do you get
  ;; if you add up the middle page number from those correctly-ordered updates?
  (let [[page-ordering-rules updates] (parse-data data)
        valid-updates (filter (fn [current-update] (valid-update? current-update page-ordering-rules)) updates)]
    (reduce + (map
               (fn [current-update]
                     (nth current-update (quot (count current-update) 2))) valid-updates))))

(defn sort-predicate-for-rule [first-page-cmp second-page-cmp page-ordering-rules]
  (let [[[applicable-rule-lesser-page applicable-rule-greater-page]]
        (filter (fn [[first-page second-page]] (or (and (= first-page-cmp first-page) (= second-page-cmp second-page))
                                                   (and (= first-page-cmp second-page) (= second-page-cmp first-page)))) page-ordering-rules)]

    (and (= first-page-cmp applicable-rule-lesser-page)
         (= second-page-cmp applicable-rule-greater-page))))

(defn correct-update [current-update page-ordering-rules]
  (if (valid-update? current-update page-ordering-rules)
    current-update
    ;; not valid, we have to fix it
    (sort (fn [first-page second-page] (sort-predicate-for-rule first-page second-page page-ordering-rules)) current-update)))

(defn part-two [data]
  (let [[page-ordering-rules updates] (parse-data data)
        invalid-updates (filter (fn [current-update] (not (valid-update? current-update page-ordering-rules))) updates)
        corrected-updates (map (fn [current-update] (correct-update current-update page-ordering-rules)) invalid-updates)]
    (reduce + (map
               (fn [current-update]
                 (nth current-update (quot (count current-update) 2))) corrected-updates))))
