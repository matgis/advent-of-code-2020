(ns day02.solution
  (:require [clojure.java.io :as io]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day02/input")))

(def ^:private input-line-regex #"(\d+)-(\d+) (.): (.+)")

(defn- first-task-valid-passwords [input-text]
  (keep (fn [[_ min-count-str max-count-str [required-letter] password-str]]
          (let [min-count (Long/parseLong min-count-str)
                max-count (Long/parseLong max-count-str)
                letter-count (bounded-count (inc max-count)
                                            (filter #(= required-letter %)
                                                    password-str))]
            (when (<= min-count letter-count max-count)
              password-str)))
        (re-seq input-line-regex input-text)))

(defn first-task
  "Find the number of valid passwords based on their respective policies. The
  password policy indicates the lowest and highest number of times a given
  letter must appear for the password to be valid."
  [input-text]
  (count (first-task-valid-passwords input-text)))

(defn- second-task-valid-passwords [input-text]
  (keep (fn [[_ first-position-str second-position-str [expected-letter] password-str]]
          (let [first-index (dec (Long/parseLong first-position-str))
                second-index (dec (Long/parseLong second-position-str))]
            (when (not= (= expected-letter (get password-str first-index))
                        (= expected-letter (get password-str second-index)))
              password-str)))
        (re-seq input-line-regex input-text)))

(defn second-task
  "Find the number of valid passwords based on their respective policies.
  Exactly one of the specified one-based positions must contain the letter."
  [input-text]
  (count (second-task-valid-passwords input-text)))
