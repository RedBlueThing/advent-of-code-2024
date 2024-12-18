(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow sqrt]])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ["Button A: X+94, Y+34"
                    "Button B: X+22, Y+67"
                    "Prize: X=8400, Y=5400"
                    ""
                    "Button A: X+26, Y+66"
                    "Button B: X+67, Y+21"
                    "Prize: X=12748, Y=12176"
                    ""
                    "Button A: X+17, Y+86"
                    "Button B: X+84, Y+37"
                    "Prize: X=7870, Y=6450"
                    ""
                    "Button A: X+69, Y+23"
                    "Button B: X+27, Y+71"
                    "Prize: X=18641, Y=10279"])


(def failing-case ["Button A: X+68, Y+30"
                   "Button B: X+14, Y+39"
                   "Prize: X=5810, Y=4193"])

(def real-data-raw (str/split-lines (slurp "day-thirteen.txt")))

(defn parse-claw-machine [data]
  ;; (Button A: X+12, Y+46 Button B: X+78, Y+29 Prize: X=7970, Y=2455)
  (let [variables [:button-a-x :button-a-y :button-b-x :button-b-y :prize-x :prize-y]]
    (into {} (->> data
                  (str/join "")
                  (re-seq #"-?\d+")
                  (map-indexed (fn [idx, value] [(nth variables idx) (Integer/parseInt value)]))))))

(defn parse-data [data] (->>
                         ;; split up our input data by empty lines
                         (partition-by #(= "" %) (map str/trim data))
                         ;; then filter out those empty lines so all we have is
                         ;; each chunk of monkey data
                         (filter #(> (count %) 1))
                         ;; parse the claw machine spec
                         (map parse-claw-machine)))

(defn gradient-for-x-y-offset [x y]
  ;; rise over run
  (/ y x))

(defn pythagorean [x y]
  ;; use Pythagorus to to work out how long the line from the origin to a
  ;; particular button offset is (so we can divide the A and B lengths and work
  ;; out how many button presses there were)
  (sqrt (+ (pow x 2) (pow y 2))))

(defn button-presses-for-length [length x y]
  (round (/ length (pythagorean x y))))

(defn normalize-vector [gradient]
  (let [norm (sqrt (+ 1 (* gradient gradient)))]
    [(/ 1.0 norm) (/ gradient norm)]))

(def maximum-presses-part-one 100)
(def maximum-presses-part-two ##Inf)

(defn calculate-a-and-b-presses [{button-a-x :button-a-x
                                  button-a-y :button-a-y
                                  button-b-x :button-b-x
                                  button-b-y :button-b-y
                                  prize-x :prize-x
                                  prize-y :prize-y}]
  (let [gradient-a (gradient-for-x-y-offset button-a-x button-a-y)
        gradient-b (gradient-for-x-y-offset button-b-x button-b-y)
        [a-x a-y] (normalize-vector gradient-a)
        [b-x b-y] (normalize-vector gradient-b)

        ;; Compute determinant
        det (- (* a-x b-y) (* a-y b-x))]

    (when (< (Math/abs det) 1e-10)
      (println "Skipping linearly dependant machine" prize-x prize-y)
      [0 0])

    ;; Compute inverse matrix
    (let [inv-00 (/ b-y det)
          inv-01 (/ (- b-x) det)
          inv-10 (/ (- a-y) det)
          inv-11 (/ a-x det)]

      ;; Solve for lengths
      (let [a-length (+ (* inv-00 prize-x) (* inv-01 prize-y))
            b-length (+ (* inv-10 prize-x) (* inv-11 prize-y))
            a-presses (button-presses-for-length a-length button-a-x button-a-y)
            b-presses (button-presses-for-length b-length button-b-x button-b-y)]
        (if (and (<= a-presses maximum-presses-part-two)
                 (<= b-presses maximum-presses-part-two)
                 (> a-presses 0)
                 (> b-presses 0))
          [a-presses b-presses]
          ;; if it is more than maximum presses on each then don't include this one
          [0 0])))))

(def a-cost 3)
(def b-cost 1)

(defn verify-presses [{button-a-x :button-a-x
                       button-a-y :button-a-y
                       button-b-x :button-b-x
                       button-b-y :button-b-y
                       prize-x :prize-x
                       prize-y :prize-y} a-presses b-presses]
  (let [calculated-x (+ (* a-presses button-a-x) (* b-presses button-b-x))
        calculated-y (+ (* a-presses button-a-y) (* b-presses button-b-y))]
    (if (and (> a-presses 0) (> b-presses 0) (= prize-x calculated-x) (= prize-y calculated-y))
      [a-presses b-presses] 
      ;; if the calculated x and calculated-y don't line up with the prize, then ignore this result.
      [0 0])))

(defn solve [claw-machines]
  (reduce + (map
             (fn [claw-machine]

               (let [[a-presses b-presses] (calculate-a-and-b-presses claw-machine)
                     [verified-a-presses verified-b-presses] (verify-presses claw-machine a-presses b-presses)]
                 (+ (* a-cost verified-a-presses) (* b-cost verified-b-presses)))) claw-machines)))

(defn part-one [data]
  (solve (parse-data data)))

(defn adjust-for-part-two [claw-machines]
  (map (fn [claw-machine]
         (into claw-machine [[:prize-x (+ (claw-machine :prize-x) 10000000000000)]
                             [:prize-y (+ (claw-machine :prize-y) 10000000000000)]])) claw-machines))

(defn part-two [data]
  (solve (adjust-for-part-two (parse-data data))))
