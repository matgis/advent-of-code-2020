(ns day15.solution
  (:require [clojure.java.io :as io]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day15/input")))

(defn- parse-numbers [input-text]
  (into (vector-of :long)
        (map #(Long/parseLong %))
        (re-seq #"\d+" input-text)))

(defn- solve-task [input-text ^long end-turn]
  (let [seed-numbers (parse-numbers input-text)]
    (loop [turn (count seed-numbers)
           ^long spoken-number (peek seed-numbers)
           game (reduce (fn [game ^long number]
                          (assoc! game number (inc (count game))))
                        (transient {})
                        (pop seed-numbers))]
      (if (>= turn end-turn)
        spoken-number
        (recur (inc turn)
               (- turn ^long (get game spoken-number turn))
               (assoc! game spoken-number turn))))))

(defn- first-task
  "Given your starting numbers, what will be the 2020th number spoken?"
  [input-text]
  (solve-task input-text 2020))

(defn- second-task
  "Given your starting numbers, what will be the 30000000th number spoken?"
  [input-text]
  (solve-task input-text 30000000))

;; -----------------------------------------------------------------------------

#_(assert (= 870 (first-task input-text)))
#_(assert (= 9136 (second-task input-text)))
