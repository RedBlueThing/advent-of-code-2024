
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

(defn file-id [index]
  (/ index 2))

(defn render-disk-map [disk-map]
  ;; probably don't do this for the real data
  (apply str (map (fn [[count file-id]] (apply str (repeat count (if (nil? file-id) "." (str file-id))))) disk-map)))

(defn parse-data [data]
  (map-indexed (fn [idx count] [count (if (file-block idx) (file-id idx) nil)]) (map #(Integer/parseInt (str %)) (first data))))

(assert (= (render-disk-map (parse-data test-data-raw)) "00...111...2...333.44.5555.6666.777.888899"))

(defn any-empty-blocks? [disk-map]
  (boolean (some true? (map (fn [[count file-id]] (nil? file-id)) disk-map))))

(defn index-of-first [pred coll]
  (some (fn [[idx item]] (when (pred item) idx)) (map-indexed vector coll)))

(defn but-last-two [vector] (subvec (vec vector) 0 (- (count vector) 2)))

(defn first-empty-block-index [disk-map]
  (index-of-first (fn [[count file-id]] (nil? file-id)) disk-map))

(defn insert-remaining-space-at-index [disk-map index remaining]
  (concat (subvec disk-map 0 (inc index)) [[remaining nil]] (subvec disk-map (inc index))))

(defn move-last-block [disk-map]
  (let [[last-block-count last-file-id] (last disk-map)]
    (loop [current-disk-map (but-last-two disk-map)
           left-to-move last-block-count
           target-index (first-empty-block-index disk-map)]
      (if (nil? target-index)
        (conj current-disk-map [left-to-move last-file-id])
        (let [[count file-id] (nth current-disk-map target-index)]
         (assert (nil? file-id))
         (if (= left-to-move 0)
          ;; done moving, we can remove the current-disk-map without the last two
          ;; (nil and the entry we moved)
           current-disk-map
          ;; keep moving
           (let [updated-disk-map (cond
                                    (= left-to-move count) (assoc current-disk-map target-index [count last-file-id])
                                    (< left-to-move count) (insert-remaining-space-at-index
                                                            (assoc current-disk-map target-index [left-to-move last-file-id])
                                                            target-index
                                                            (- count left-to-move))
                                    (> left-to-move count) (assoc current-disk-map target-index [count last-file-id]))
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

(defn first-empty-block-index-that-fits-and-is-not-the-source [disk-map count file-id]
  ;; we want the index of the first block we can find that has a count larger
  ;; than the count we checking
  (let [source-index (index-of-first (fn [[maybe-source-file-count maybe-source-file-id]] (= maybe-source-file-id file-id)) disk-map)
        index-of-first-block-that-fits (index-of-first (fn [[file-count file-id]]
                                                         (if (nil? file-id)
                                                           (<= count file-count)
                                                           false)) disk-map)]
    (if (and (not (nil? index-of-first-block-that-fits))
             (< index-of-first-block-that-fits source-index))
      [ index-of-first-block-that-fits source-index ] [ nil nil ])))

(assert (= (first-empty-block-index-that-fits-and-is-not-the-source [[2 0] [3 nil] [3 1] [3 nil] [1 2] [3 nil] [3 3] [1 nil] [2 4] [1 nil] [4 5] [1 nil] [4 6] [1 nil] [3 7] [1 nil] [4 8] [0 nil] [2 9]] 2 9) [1 18]))
(assert (= (first-empty-block-index-that-fits-and-is-not-the-source [[2 0] [3 nil] [3 1] [3 nil] [1 2] [3 nil] [3 3] [1 nil] [2 4] [1 nil] [4 5] [1 nil] [4 6] [1 nil] [3 7] [1 nil] [4 8] [0 nil] [2 9]] 3 7) [1 14]))

(defn remove-at [v index] (vec (concat (subvec (vec v) 0 index) (subvec (vec v) (inc index)))))

;; move whole files
;;
;; [[2 0] [3 nil] [3 1] [3 nil] [1 2] [3 nil] [3 3] [1 nil] [2 4] [1 nil] [4 5] [1 nil] [4 6] [1 nil] [3 7] [1 nil] [4 8] [0 nil] [2 9]]
;; "00...111...2...333.44.5555.6666.777.888899"
;;
;; to
;;
;; [[2 0] [2 9] [1 2] [3 1] [3 7] [1 nil] [2 4] [1 nil] [3 3] [1 nil] [2 nil] [1 nil] [4 5] [1 nil] [4 6] [1 nil] [3 nil] [1 nil] [4 8] [0 nil] [2 nil]]
;; "00992111777.44.333....5555.6666.....8888.."

(defn move-whole-files [disk-map]
  (reduce (fn [current-disk-map [idx [count file-id]]]
            (let [[ target-index source-index ] (first-empty-block-index-that-fits-and-is-not-the-source current-disk-map count file-id)]
              ;; if the target nil doesn't exist (or one is a nil that we don't need to move)
              (if (or (nil? target-index) (nil? file-id))

                ;; if we can't find a spot to fit this file-id
                current-disk-map

                ;; we found a spot it fits, need to optionally insert a nil
                ;; entry with the remainder (if any)
                (let [[target-count target-file-id] (nth current-disk-map target-index)
                      ;; we need a version of the current disk map with the
                      ;; block we are moving removed. We can do that because we
                      ;; know the item is on the right of the target-index (so
                      ;; it won't break that index)
                      updated-disk-map (assoc (vec current-disk-map) source-index [count nil])]
                  (assert (nil? target-file-id))
                  (assert (<= count target-count))
                  (cond (= count target-count) (assoc (vec updated-disk-map) target-index [count file-id])
                        :else (insert-remaining-space-at-index
                               (assoc (vec updated-disk-map) target-index [count file-id])
                               target-index
                               (- target-count count)))))))

          disk-map (map-indexed (fn [idx block-data] [idx block-data]) (reverse disk-map))))

(defn checksum-for-contiguous-segment [check-sum-acc [count file-id]]
  (let [{running-checksum :running-checksum segment-disk-position-index :segment-disk-position-index} check-sum-acc]
    (if (nil? file-id)
      {:running-checksum running-checksum :segment-disk-position-index (+ segment-disk-position-index count)}
      (let [segment-checksum (reduce + (map (fn [block-index]
                                              (* file-id (+ segment-disk-position-index block-index)))
                                            (range 0 count)))]
        {:running-checksum (+ running-checksum segment-checksum) :segment-disk-position-index (+ segment-disk-position-index count)}))))

(defn calculate-checksum [compact-disk-map]
  ;; To calculate the checksum, add up the result of multiplying each of these
  ;; blocks' position with the file ID number it contains. Where a compact disk map
  ;; looks like (a sequence of contiguous segment definitions):
  ;;
  ;; [[count file-id] [count file-id]]
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

;; part one
(calculate-checksum (de-fragment (parse-data test-data-raw)))

;; part two
(calculate-checksum (move-whole-files (parse-data test-data-raw)))
