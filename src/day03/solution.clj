(ns day03.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day03/input")))

(defn- count-trees [input-lines [^long x-start ^long y-start] [^long x-delta ^long y-delta]]
  (let [line-count (count input-lines)
        is-tree? (fn is-tree? [x y]
                   (let [line (nth input-lines y)
                         line-length (count line)
                         letter-index (mod x line-length)
                         letter (nth line letter-index)]
                     (= \# letter)))]
    (loop [x x-start
           y y-start
           tree-count 0]
      (if (<= line-count y)
        tree-count
        (recur (+ x x-delta)
               (+ y y-delta)
               (if (is-tree? x y)
                 (inc tree-count)
                 tree-count))))))

(defn first-task
  "Count the number of trees encountered on the toboggan slope right 3, down 1."
  [input-text]
  (let [input-lines (string/split-lines input-text)]
    (count-trees input-lines [0 0] [3 1])))

(defn second-task
  "Count the product of the number of trees encountered on the toboggan slopes:
     Right 1, down 1.
     Right 3, down 1. (This is the slope you already checked.)
     Right 5, down 1.
     Right 7, down 1.
     Right 1, down 2."
  [input-text]
  (let [input-lines (string/split-lines input-text)]
    (reduce *
            1
            (map (partial count-trees input-lines [0 0])
                 [[1 1]
                  [3 1]
                  [5 1]
                  [7 1]
                  [1 2]]))))
