
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])

(def test-data-raw-part-one ["xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"])

(def test-data-raw-part-two ["xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"])

(def real-data-raw (str/split-lines (slurp "day-three.txt")))

(defn multiplications-for-line [line]
  (let [split-by-mul (str/split line #"mul\(")
        matching-groups (filter some? (map (fn [segment] (re-matches #"(\d+),(\d+)\)(.*)" segment)) split-by-mul))]
    (reduce + (map (fn [[segment first-value second-value rest]] (* (Integer/parseInt first-value) (Integer/parseInt second-value))) matching-groups))))

(defn part-one [data]
  (reduce + (map multiplications-for-line data)))

(def do-not-command "don't()")
(def do-command "do()")


(defn do-or-do-not-for-segment [multiplcations-enabled segment]
  (let [last-do-not-index (str/last-index-of segment do-not-command)
        last-do-index (str/last-index-of segment do-command)]
    (let [new-multiplcations-enabled (if (or last-do-index last-do-not-index)
                                       ;; whichever is larger
                                       (cond
                                         (and last-do-index last-do-not-index) (if (> last-do-index last-do-not-index) true false)
                                         (some? last-do-index) true
                                         (some? last-do-not-index) false)
                                       ;; neither
                                       multiplcations-enabled)]
      new-multiplcations-enabled)))

(defn enabled-multiplications-for-line [line default-multiplications-enabled]
  (let [split-by-mul (str/split line #"mul\(")]
    (loop [idx 0
           current-multiplication-sum 0
           multiplcations-enabled default-multiplications-enabled]
      (if (= (count split-by-mul) idx)
        [multiplcations-enabled current-multiplication-sum]
        (let [segment (nth split-by-mul idx)
              multiplication-groups (re-matches #"(\d+),(\d+)\)(.*)" segment)
              [new-multiplication-sum new-multiplications-enabled] (if (nil? multiplication-groups)
                                                                     ;; need to check for do and do-not commands on the segment
                                                                     [current-multiplication-sum (do-or-do-not-for-segment multiplcations-enabled segment)]
                                                                     (let [[segment first-value second-value rest] multiplication-groups]
                                                                       ;; need to grab the multiplication value and check for do and do-not commands on the rest
                                                                       [(if multiplcations-enabled
                                                                          (+ current-multiplication-sum (* (Integer/parseInt first-value) (Integer/parseInt second-value)))
                                                                          current-multiplication-sum)
                                                                        (do-or-do-not-for-segment multiplcations-enabled rest)]))]

          (recur (inc idx) new-multiplication-sum new-multiplications-enabled))))))

(defn part-two [data]
  (loop [current-multiplication-sum 0
         multiplications-enabled true
         idx 0]
    (if (= (count data) idx)
      current-multiplication-sum
      (let [[new-multiplcations-enabled new-multiplication-sum] (enabled-multiplications-for-line (nth data idx) multiplications-enabled)]
        (recur (+ current-multiplication-sum new-multiplication-sum) new-multiplcations-enabled (inc idx))))))
