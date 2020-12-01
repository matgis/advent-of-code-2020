(ns day01.solution
  (:require [clojure.java.io :as io])
  (:import (java.io BufferedReader StringReader)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day01/input")))

(defn- string-reader [string]
  (BufferedReader. (StringReader. string)))

(defn- make-expenses-set [input-text]
  (into #{}
        (map #(Long/parseLong %))
        (line-seq (string-reader input-text))))

(defn first-task
  "Find the product of the two entries that sum to 2020."
  [input-text]
  (let [expenses-set (make-expenses-set input-text)]
    (some (fn [^long expense]
            (let [remainder (- 2020 expense)]
              (when (expenses-set remainder)
                (* expense remainder))))
          expenses-set)))

(defn second-task
  "Find the product of the three entries that sum to 2020."
  [input-text]
  (let [expenses-set (make-expenses-set input-text)]
    (first
      (for [^long first-expense expenses-set
            ^long second-expense expenses-set
            :when (not= first-expense second-expense)
            :let [remainder (- 2020 (+ first-expense second-expense))]
            :when (expenses-set remainder)]
        (* first-expense second-expense remainder)))))
