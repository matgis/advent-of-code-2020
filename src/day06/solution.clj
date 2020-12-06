(ns day06.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
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

(defn first-task
  "Sum the number of questions for which anyone in each group answers 'yes'."
  [input-text]
  (->> input-text
       (parse-groups)
       (map (partial into #{} cat))
       (map count)
       (reduce +)))

(def ^:private questions-set (set "abcdefghijklmnopqrstuvwxyz"))

(defn second-task
  "Sum the number of questions for which everyone in each group answers 'yes'."
  [input-text]
  (->> input-text
       (parse-groups)
       (map (fn [group-yes-answers]
              (transduce (map set)
                         set/intersection
                         questions-set
                         group-yes-answers)))
       (map count)
       (reduce +)))
