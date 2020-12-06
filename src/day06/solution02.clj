(ns day06.solution02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day06/input")))

(defn- parse-groups [input-text]
  (->> input-text
       (string/split-lines)
       (partition-by empty?)
       (remove (comp empty? first))))

(defn- bit-and'
  (^long [] (bit-not 0))
  (^long [^long a] a)
  (^long [^long a ^long b] (bit-and a b)))

(defn- bit-or'
  (^long [] 0)
  (^long [^long a] a)
  (^long [^long a ^long b] (bit-or a b)))

(defn- bit-count
  ^long [^long a]
  (Integer/bitCount a))

(defn- letter-distance
  ^long [from to]
  (- (int to) (int from)))

(defn- letter->bit
  ^long [letter]
  (bit-shift-left 1 (letter-distance \a letter)))

(defn- person->bits
  ^long [person]
  (transduce (map letter->bit) bit-or' person))

(defn- group->bits
  ^long [bit-combine-fn group]
  (transduce (map person->bits) bit-combine-fn group))

(defn- solve-task [input-text group-bits-fn]
  (let [group->bit-count (comp bit-count group-bits-fn)]
    (transduce (map group->bit-count) + (parse-groups input-text))))

(defn- first-task
  "Sum the number of questions for which anyone in each group answers 'yes'."
  [input-text]
  (solve-task input-text (partial group->bits bit-or')))

(defn- second-task
  "Sum the number of questions for which everyone in each group answers 'yes'."
  [input-text]
  (solve-task input-text (partial group->bits bit-and')))
