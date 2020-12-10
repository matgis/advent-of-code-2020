(ns day10.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day10/input")))

(defn- sorted-adapter-joltages [input-text]
  (->> input-text
       (string/split-lines)
       (map #(Long/parseLong %))
       (sort)))

(defn- joltage-chain [input-text]
  (let [sorted-adapter-joltages (sorted-adapter-joltages input-text)
        device-joltage (+ 3 ^long (last sorted-adapter-joltages))
        outlet-joltage 0]
    (conj (into (vector-of :long outlet-joltage)
                sorted-adapter-joltages)
          device-joltage)))

(defn- differences [joltage-chain]
  (map - (drop 1 joltage-chain) joltage-chain))

(defn- first-task
  "Find a chain that uses all of your adapters to connect the charging outlet to
  your device's built-in adapter and count the joltage differences between the
  charging outlet, the adapters, and your device. What is the number of 1-jolt
  differences multiplied by the number of 3-jolt differences?"
  [input-text]
  (->> input-text
       (joltage-chain)
       (differences)
       (frequencies)
       (vals)
       (reduce *)))

(defn- second-task
  "What is the total number of distinct ways you can arrange the adapters to
  connect the charging outlet to your device?"
  [input-text]
  (->> input-text
       (joltage-chain)
       (differences)
       (partition-by #{3})
       (filter (comp #{1} first))
       (map (comp {1 1, 2 2, 3 4, 4 7} count))
       (reduce *)))
