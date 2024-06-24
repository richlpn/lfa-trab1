(ns clirc.bool-logic
  (:require [clojure.math :refer :all]))


(defn bit-get
  "Returns the `n`th bit of the number `x` when `x` is represented in binary."
  [x n]
  (if (bit-test x n) 1 0))


(defn log2
  "Return the logarithm of `x` in base 2."
  [x]
  (/ (log x) (log 2)))


(defn min-bitvec-size
  "Returns the minimum number of bits needed to represent `x` in binary."
  [x]
  {:pre [(not (neg? x))]}
  (cond
    (zero? x) 1
    :else (inc (int (floor (log2 x))))))


(defn int->bitvec
  "Converts a non-negative integer to the bit vector (0 and 1) corresponding. The
  resulting vector is 'LSB first', i.e., the least bit significant appears at
  position zero of the vector.

   - `x` is the number to be converted.

   - `n` is the size of the resulting vector. If not specified, it is calculated
  as the smallest size needed to represent `x`. If specified for a value smaller
  than necessary to represent `x`, the conversion will be truncated."
  ([x]
   {:pre [(not (neg? x))]}
   (let [n (min-bitvec-size x)]
     (int->bitvec x n)))
  ([x n]
   {:pre [(not (neg? x)) (pos? n)]}
   (into [] (for [i (range n)] (bit-get x i)))))


(defn map->bitvec
  "Converts a map to a vector. The keys of the map as assumed to correspond to
  indexes of the vector. Indexes with no value in the map will hold `nil` in the
  returned vector. Therefore, for example, the map `{1 0, 2 0, 4 1, 7 1}` will
  be converted to the vector `[nil 0 0 nil 1 nil nil 1]`."
  [m]
  (if (empty? m)
    []
    (let [imax (apply max (keys m))]
      (into [] (for [i (range (inc imax))] (get m i nil))))))


(defn bitvec-range
  "Returns a sequence of bitvecs representing the integer range defined. The
  `step` cannot be zero, and if `start` is lower than `end`, then `step` must be
  negative."
  ([end] (bitvec-range 0 end 1))
  ([start end]
   {:pre [(not (neg? start)) (not (neg? end))]}
   (bitvec-range start end (if (< start end) -1 1)))
  ([start end step]
   {:pre [(not (neg? start))
          (not (neg? end))
          (not (zero? step))
          (or (and (>= end start) (pos? step))
              (and (< end start) (neg? step)))]}
   (let [size (min-bitvec-size (max start end))]
     (loop [acc [] curr (if (pos? step) start end)]
       (cond
         (and (pos? step) (>= curr end)) acc
         (and (neg? step) (<= curr start)) acc
         :else (let [v0 (into [] (reverse (int->bitvec curr size)))]
                 (recur (conj acc v0) (+ curr step))))))))


(defn truth-table
  [ncols funcs]
  (let [m (int (pow 2 ncols))
        inputs (bitvec-range m)]
    (for [row inputs]
      (for [f funcs] (apply f row)))))
