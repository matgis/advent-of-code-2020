(ns day05.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day05/input")))

(defn- find-seat-id
  "Parse a seat key in the format 'BFFFBBFRRR' to binary 1000110111 = 567."
  ^long [^String seat-key]
  (loop [letter-index 0
         seat-id 0]
    (if (< letter-index 10)
      (recur (inc letter-index)
             (case (.charAt seat-key letter-index)
               (\B \R) (long (bit-set seat-id (- 9 letter-index)))
               seat-id))
      seat-id)))

(defn- taken-seat-ids [input-text]
  (map find-seat-id (string/split-lines input-text)))

(defn first-task
  "Find the highest taken seat-id by binary-space partitioning."
  [input-text]
  (reduce max (taken-seat-ids input-text)))

(defn second-task
  "Find the only available seat-id, excluding invalid seats at the outer range."
  [input-text]
  (->> input-text
       (taken-seat-ids)
       (sort)
       (reduce (fn [^long prev-taken-seat-id ^long next-taken-seat-id]
                 (let [next-seat-id (inc prev-taken-seat-id)]
                   (if (= next-taken-seat-id next-seat-id)
                     next-taken-seat-id
                     (reduced next-seat-id)))))))
