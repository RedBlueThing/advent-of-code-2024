
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw ["3   4"
                    "4   3"
                    "2   5"
                    "1   3"
                    "3   9"
                    "3   3"])

(def real-data-raw (str/split-lines (slurp "day-one.txt")))

(def expected-part-one-test-result 11)

(defn parse-data [lines]
  (reduce (fn [[first-list second-list] line]
            (let [integers (map #(Integer/parseInt %) (str/split line #"\s+"))]
              [(conj first-list (first integers))
               (conj second-list (second integers))])) [[] []] lines))

(defn part-one [data]
  (let [[first-list second-list] (parse-data data)
        [sorted-first-list sorted-second-list] [(sort first-list) (sort second-list)]
        combined-lists (map vector sorted-first-list sorted-second-list)]
    (reduce (fn [total [first-number second-number]] (+ (Math/abs (- first-number second-number)) total)) 0 combined-lists)))

(def expected-part-two-test-result 31)

(defn appear-count [number other-list]
  (count (filter #(= % number) other-list)))

(defn part-two [data]
  (let [[first-list second-list] (parse-data data)]
    (reduce (fn [total current-first-list-value] (+ total (* current-first-list-value (appear-count current-first-list-value second-list)))) 0 first-list)))
