(ns day07.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day07/input")))

(defn- string->keyword [string]
  (keyword (string/replace string " " "-")))

(defn- parse-rules [input-text]
  (into {}
        (map (fn [[_ rule-color-str rest-str]]
               [(string->keyword rule-color-str)
                (into {}
                      (map (fn [[_ required-amount-str required-color-str]]
                             [(string->keyword required-color-str)
                              (Long/parseLong required-amount-str)]))
                      (re-seq #"(\d+) (.+?)(?> bags?)(?>, |\.)"
                              rest-str))]))
        (re-seq #"(.+?) bags contain (.+)"
                input-text)))

(defn- valid-outermost-bags [rules innermost-bag-color]
  (into #{}
        (mapcat (fn [[outer-bag-color required-amounts-by-color]]
                  (when (contains? required-amounts-by-color innermost-bag-color)
                    (cons outer-bag-color
                          (valid-outermost-bags rules outer-bag-color)))))
        rules))

(defn- first-task
  "How many colors can, eventually, contain at least one shiny gold bag?"
  [input-text]
  (count
    (valid-outermost-bags (parse-rules input-text)
                          :shiny-gold)))

(defn- required-bags [rules outer-bag-color]
  (cons outer-bag-color
        (mapcat (partial required-bags rules)
                (mapcat (fn [[inner-bag-color amount]]
                          (repeat amount inner-bag-color))
                        (rules outer-bag-color)))))

(defn- second-task
  "How many individual bags are required inside your single shiny gold bag?"
  [input-text]
  (dec ; Don't count the outermost shiny gold bag.
    (count
      (required-bags (parse-rules input-text)
                     :shiny-gold))))
