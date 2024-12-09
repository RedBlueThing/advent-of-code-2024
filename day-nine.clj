
(require '[clojure.string :as str])
(require '[clojure.set :as set])
(require '[clojure.math :refer [ceil floor round pow]])
(require '[clojure.math.combinatorics :as combo])

(def test-data-raw ["2333133121414131402"])

(def real-data-raw (str/split-lines (slurp "day-nine.txt")))

(defn raw-data [data]
  ;; just one line
  (map #(Integer/parseInt (str %)) (first data)))

(defn file-block [index]
  (= 0 (mod index 2)))

(defn block-id [index]
  (/ index 2))

(defn render-disk-map [disk-map]
  ;; probably don't do this for the real data
  (apply str (map (fn [[count block-id]] (apply str (repeat count (if (nil? block-id) "." (str block-id))))) disk-map)))

(defn parse-data [data]
  (map-indexed (fn [idx count] [count (if (file-block idx) (block-id idx) nil)]) (map #(Integer/parseInt (str %)) (first data))))

(assert (= (render-disk-map (parse-data test-data-raw)) "00...111...2...333.44.5555.6666.777.888899"))

(defn any-empty-blocks? [disk-map]
  (boolean (some true? (map (fn [[count block-id]] (nil? block-id)) disk-map))))

(defn index-of-first [pred coll]
  (some (fn [[idx item]] (when (pred item) idx)) (map-indexed vector coll)))

(defn but-last-two [vector] (subvec (vec vector) 0 (- (count vector) 2)))

(defn first-empty-block-index [disk-map]
  (index-of-first (fn [[count block-id]] (nil? block-id)) disk-map))

(defn insert-remaining-space-at-index [disk-map index remaining]
  (concat (subvec disk-map 0 (inc index)) [[remaining nil]] (subvec disk-map (inc index))))

(defn move-last-block [disk-map]
  (let [[last-block-count last-block-id] (last disk-map)]
    (loop [current-disk-map (but-last-two disk-map)
           left-to-move last-block-count
           target-index (first-empty-block-index disk-map)]
      (if (nil? target-index)
        (conj current-disk-map [left-to-move last-block-id])
        (let [[count block-id] (nth current-disk-map target-index)]
         (assert (nil? block-id))
         (if (= left-to-move 0)
          ;; done moving, we can remove the current-disk-map without the last two
          ;; (nil and the entry we moved)
           current-disk-map
          ;; keep moving
           (let [updated-disk-map (cond
                                    (= left-to-move count) (assoc current-disk-map target-index [count last-block-id])
                                    (< left-to-move count) (insert-remaining-space-at-index
                                                            (assoc current-disk-map target-index [left-to-move last-block-id])
                                                            target-index
                                                            (- count left-to-move))
                                    (> left-to-move count) (assoc current-disk-map target-index [count last-block-id]))
                 updated-left-to-move (cond
                                        (= left-to-move count) 0
                                        (< left-to-move count) 0
                                        (> left-to-move count) (- left-to-move count))]
             (recur updated-disk-map updated-left-to-move (first-empty-block-index updated-disk-map)))))))))

;; de-fragment
;;
;; [[2 0] [3 nil] [3 1] [3 nil] [1 2] [3 nil] [3 3] [1 nil] [2 4] [1 nil] [4 5] [1 nil] [4 6] [1 nil] [3 7] [1 nil] [4 8] [0 nil] [2 9]]
;;
;; to
;;
;; [[2 0] [2 9] [1 8] [3 1] [3 8] [1 2] [3 7] [3 3] [1 6] [2 4] [1 6] [4 5] [2 6]]

(defn de-fragment [disk-map]
  (loop [current-disk-map disk-map]
    ;; any space to move blocks?
    (if (not (any-empty-blocks? current-disk-map))
      current-disk-map
      (recur (move-last-block current-disk-map)))))

(defn checksum-for-contiguous-segment [{running-checksum :running-checksum segment-disk-position-index :segment-disk-position-index}
                                       [count block-id]]
  ;; need a de-fragmented disk-map
  (assert (not (nil? block-id)))
  (let [segment-checksum (reduce + (map (fn [block-index]
                                          (* block-id (+ segment-disk-position-index block-index)))
                                        (range 0 count)))]
    {:running-checksum (+ running-checksum segment-checksum) :segment-disk-position-index (+ segment-disk-position-index count)}))

(defn calculate-checksum [compact-disk-map]
  ;; To calculate the checksum, add up the result of multiplying each of these
  ;; blocks' position with the file ID number it contains. Where a compact disk map
  ;; looks like (a sequence of contiguous segment definitions):
  ;;
  ;; [[count block-id] [count block-id]]
  (reduce checksum-for-contiguous-segment {:running-checksum 0, :segment-disk-position-index 0} compact-disk-map))

(assert (= ((calculate-checksum [[2 0]
                               [2 9]
                               [1 8]
                               [3 1]
                               [3 8]
                               [1 2]
                               [3 7]
                               [3 3]
                               [1 6]
                               [2 4]
                               [1 6]
                               [4 5]
                               [2 6]]) :running-checksum) 1928))
