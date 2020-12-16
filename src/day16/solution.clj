(ns day16.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day16/input")))

(defn- parse-long
  ^long [str]
  (Long/parseLong str))

(defn- parse-ticket [ticket-str]
  (into (vector-of :long)
        (map parse-long)
        (string/split ticket-str #",")))

(defn- make-validation-fn [range-limits]
  (let [[^long sa ^long ea ^long sb ^long eb] (map parse-long range-limits)]
    (fn valid-field-value? [^long field-value]
      (or (<= sa field-value ea)
          (<= sb field-value eb)))))

(defn- parse-field-name [field-name-str]
  (keyword (string/replace field-name-str " " "-")))

(defn- parse-rules [rules-str]
  (into {}
        (map (fn [[_ field-name & range-limits]]
               [(parse-field-name field-name)
                (make-validation-fn range-limits)]))
        (re-seq #"(.+?): (\d+)-(\d+) or (\d+)-(\d+)"
                rules-str)))

(defn- parse-input [input-text]
  (let [[rules-str my-ticket-str nearby-tickets-str] (string/split input-text #"\n{2}.+:\n")]
    {:rules (parse-rules rules-str)
     :my-ticket (parse-ticket my-ticket-str)
     :nearby-tickets (map parse-ticket (string/split-lines nearby-tickets-str))}))

(defn- first-task
  "Add together all of the values that are invalid for every field rule from all
  nearby tickets."
  [input-text]
  (let [{:keys [nearby-tickets rules]} (parse-input input-text)
        valid-value-for-some-field? (fn [field-value]
                                      (some (fn [[_field-name valid-field-value?]]
                                              (valid-field-value? field-value))
                                            rules))
        find-invalid-field-values (partial remove valid-value-for-some-field?)]
    (transduce (mapcat find-invalid-field-values) + nearby-tickets)))

(defn- valid-ticket? [rules ticket]
  (every? (fn [^long field-value]
            (some (fn [[_field-name valid-field-value?]]
                    (valid-field-value? field-value))
                  rules))
          ticket))

(defn- field-name-candidates-per-field [rules tickets]
  (into []
        (map (comp set keys))
        (reduce (fn [passing-rules-per-field field-values]
                  (map (fn [passing-field-rules field-value]
                         (into {}
                               (filter (fn [[_field-name valid-field-value?]]
                                         (valid-field-value? field-value)))
                               passing-field-rules))
                       passing-rules-per-field
                       field-values))
                (vec (repeat (count rules) rules))
                tickets)))

(defn- only [coll]
  (when (= 1 (count coll))
    (first coll)))

(defn- solve-field-names [field-name-candidates-per-field]
  (let [field-count (count field-name-candidates-per-field)
        solved-field-names (into #{} (keep only) field-name-candidates-per-field)]
    (if (= field-count (count solved-field-names))
      (mapv first field-name-candidates-per-field)
      (recur (mapv (fn [field-name-candidates]
                     (if (= 1 (count field-name-candidates))
                       field-name-candidates
                       (set/difference field-name-candidates solved-field-names)))
                   field-name-candidates-per-field)))))

(defn- second-task
  "Multiply together all the values from field names that start with 'departure'
  on your ticket."
  [input-text]
  (let [input (parse-input input-text)
        rules (:rules input)
        nearby-tickets (filter (partial valid-ticket? rules) (:nearby-tickets input))
        field-name-candidates-per-field (field-name-candidates-per-field rules nearby-tickets)
        field-names (solve-field-names field-name-candidates-per-field)
        labelled-ticket (zipmap field-names (:my-ticket input))
        departure-field? (comp #(string/starts-with? % "departure") name key)]
    (transduce (comp (filter departure-field?)
                     (map val))
               *
               labelled-ticket)))

;; -----------------------------------------------------------------------------

(assert (= 22057 (first-task input-text)))
(assert (= 1093427331937 (second-task input-text)))
