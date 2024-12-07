
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ["190: 10 19"
                    "3267: 81 40 27"
                    "83: 17 5"
                    "156: 15 6"
                    "7290: 6 8 6 15"
                    "161011: 16 10 13"
                    "192: 17 8 14"
                    "21037: 9 7 18 13"
                    "292: 11 6 16 20"])

(def real-data-raw (str/split-lines (slurp "day-seven.txt")))

(defn parse-data [data]
  (map (fn [line] (let [[test-value-str equation-str] (str/split line #": ")
                        test-value (Long/parseLong test-value-str)
                        equation-values (map #(Long/parseLong %) (str/split equation-str #" "))]
                    [test-value equation-values])) data))


(defn apply-operations [equation-values operations]
  (reduce (fn [acc [op val]]
            (case op
              :sum (+ acc val)
              :mul (* acc val)
              :concat (Long/parseLong (str (str acc) (str val)))
              ))
          (first equation-values) (map vector operations (rest equation-values))))

(defn all-operations [required-operations]
  (let [ops [:sum :mul :concat]]
    (reduce (fn [acc _] (for [combo acc op ops] (conj combo op)))
            [[]] (range required-operations))))

(defn valid-equation? [[test-value equation-values]]
  (let [all-operations (all-operations (dec (count equation-values)))
        all-results (map #(apply-operations equation-values %) all-operations)]
    (some true? (map #(=  % test-value) all-results))))

(defn filter-for-valid-equations [equations]
  (filter valid-equation? equations))

(defn sum-of-valid-equations [equations]
  (reduce + (map first equations)))

(defn part-one [data]
  (-> data
      parse-data
      filter-for-valid-equations
      sum-of-valid-equations))

