(ns magic-square.puzzle
  "This puzzle comes from Lewis Carroll.  The magic part is when the
   values on a square are arranged so that adding them up in any direction results in
   a constant sum.

   You need to arrange them in a 3 x 3 matrix so that:

   1. The sums of numbers in each row = magic number
   2. The sums of numbers in each column = magic number
   3. The sums of numbers in each diagonal = magic number"
  (:require [clojure.math.combinatorics :refer [combinations permutations]]))

(def values
  "The values for the magic square puzzle."
  [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- sum
  "Computes the sum of a given collection."
  [coll]
  (reduce + coll))

(defn- sum-rows
  "Sums the rows of the 3x3 square."
  [square]
  (map sum square))

(defn- sum-cols
  "Sums the columns of the 3x3 square."
  [square]
  (let [first-col (map first square)
        second-col (map second square)
        third-col (map last square)]
    [(sum first-col) (sum second-col) (sum third-col)]))

(defn- sum-diagonals
  "Sums the diagnals of the 3x3 square."
  [square]
  (let [left-to-right-diagonals [(first (first square)) (second (second square)) (last (last square))]
        right-to-left-diagonals [(first (last square)) (second (second square)) (last (first square))]]
    [(sum left-to-right-diagonals) (sum right-to-left-diagonals)]))

(defn- valid-square?
  "Checks to see if the square is valid"
  [square]
  (= (set (sum-rows square))
     (set (sum-cols square))
     (set (sum-diagonals square))))

(defn magic-square
  "Solves the magic square given a set of values."
  [values]
  (->> (permutations values)                                ; Get all permutations
       (map #(map vec (partition 3 %)))                     ; Partition each permutation into a 3x3 square
       (filter #(valid-square? %))                          ; Filter only the valid square
       first                                                ; Find the first valid square
       vec))                                                ; Convert it into a vector
