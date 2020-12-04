(ns day04.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day04/input")))

(defn- parse-entries [input-text]
  (->> input-text
       (string/split-lines)
       (partition-by empty?)
       (map #(string/join " " %))
       (remove empty?)
       (map #(string/split % #" "))
       (map (partial into {}
                     (map (fn [key:value-string]
                            (let [[key-string value-string] (string/split key:value-string #":")]
                              [(keyword key-string) value-string])))))))

(def ^:private first-task-valid-entry? (partial set/subset? #{:byr :iyr :eyr :hgt :hcl :ecl :pid}))

(defn- first-task-valid-passports [input-text]
  (filter first-task-valid-entry? (parse-entries input-text)))

(defn first-task
  "Count the number of valid passports.
     byr (Birth Year)
     iyr (Issue Year)
     eyr (Expiration Year)
     hgt (Height)
     hcl (Hair Color)
     ecl (Eye Color)
     pid (Passport ID)
     cid (Country ID) - Optional"
  [input-text]
  (count (first-task-valid-passports input-text)))

(defn- valid-byr? [byr]
  (and (re-matches #"\d{4}" byr)
       (<= 1920 (Long/parseLong byr) 2002)))

(defn- valid-iyr? [iyr]
  (and (re-matches #"\d{4}" iyr)
       (<= 2010 (Long/parseLong iyr) 2020)))

(defn- valid-eyr? [eyr]
  (and (re-matches #"\d{4}" eyr)
       (<= 2020 (Long/parseLong eyr) 2030)))

(defn- valid-hgt? [hgt]
  (when-some [[_ height-str unit] (re-matches #"(\d+)(cm|in)" hgt)]
    (case unit
      "cm" (<= 150 (Long/parseLong height-str) 193)
      "in" (<= 59 (Long/parseLong height-str) 76))))

(defn- valid-hcl? [hcl]
  (re-matches #"#[0-9a-f]{6}" hcl))

(def ^:private valid-ecl? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn- valid-pid? [pid]
  (re-matches #"\d{9}" pid))

(defn- second-task-valid-entry? [entry]
  (and (some-> entry :byr valid-byr?)
       (some-> entry :iyr valid-iyr?)
       (some-> entry :eyr valid-eyr?)
       (some-> entry :hgt valid-hgt?)
       (some-> entry :hcl valid-hcl?)
       (some-> entry :ecl valid-ecl?)
       (some-> entry :pid valid-pid?)))

(defn- second-task-valid-passports [input-text]
  (filter second-task-valid-entry? (parse-entries input-text)))

(defn second-task
  "Count the number of valid passports, validating field values."
  [input-text]
  (count (second-task-valid-passports input-text)))
