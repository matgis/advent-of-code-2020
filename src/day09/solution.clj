(ns day09.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day09/input")))

(defn- parse-numbers [input-text]
  (into (vector-of :long)
        (map #(Long/parseLong %))
        (string/split-lines input-text)))

(defn- find-sum-terms-in-range [numbers ^long preamble-count ^long index]
  (let [preamble-start-index (- index preamble-count)
        preamble-index-range (range preamble-start-index index)
        sum-at-index (numbers index)]
    (for [^long a-index preamble-index-range
          ^long b-index preamble-index-range
          :when (not= a-index b-index)
          :let [^long a (numbers a-index)
                ^long b (numbers b-index)]
          :when (= sum-at-index (+ a b))]
      (vector-of :long a b))))

(defn- solve-first-task
  ^long [preamble-count numbers]
  (some (fn [index]
          (when (empty? (find-sum-terms-in-range numbers preamble-count index))
            (numbers index)))
        (range preamble-count
               (count numbers))))

(defn- first-task
  "Find the first number in the list (after the preamble) which is not the sum
  of two of the 25 numbers before it."
  [input-text]
  (solve-first-task 25 (parse-numbers input-text)))

(defn- find-contiguous-sum-terms [numbers sum]
  (let [not-exceeding-sum? (partial >= sum)]
    (some (fn [^long start-index]
            (let [terms (drop start-index numbers)
                  sums (take-while not-exceeding-sum? (reductions + terms))]
              (when (= sum (last sums))
                (take (count sums) terms))))
          (range (count numbers)))))

(defn solve-second-task [preamble-count numbers]
  (let [invalid-number (solve-first-task preamble-count numbers)
        contiguous-sum-terms (find-contiguous-sum-terms numbers invalid-number)
        ^long smallest-number (reduce min Long/MAX_VALUE contiguous-sum-terms)
        ^long largest-number (reduce max Long/MIN_VALUE contiguous-sum-terms)]
    (+ smallest-number largest-number)))

(defn- second-task
  "Find the contiguous range of numbers that sum to the number from the first
  task. Add together the smallest and largest number in this contiguous range."
  [input-text]
  (solve-second-task 25 (parse-numbers input-text)))
