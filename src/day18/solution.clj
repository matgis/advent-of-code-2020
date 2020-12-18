(ns day18.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day18/input")))

(def ^:private tokens-re #"(\d+|[()+\-\/*])")

(defn tokenize-one [str]
  (case str
    ("+" "-" "*" "/" "(" ")") (first str)
    (Long/parseLong str)))

(defn- tokenize [expression-str]
  (map (comp tokenize-one first)
       (re-seq tokens-re expression-str)))

(declare ^:private parse-expression)

(defn- parse-group [operator->precedence tokens]
  (let [[expression [_right-paren-token & rest-tokens]] (parse-expression operator->precedence tokens 0)]
    [expression rest-tokens]))

(defn- parse-prefix [operator->precedence [token & rest-tokens]]
  (if (= \( token)
    (parse-group operator->precedence rest-tokens)
    [token rest-tokens]))

(defn- parse-infix [operator->precedence left-expression operator-token right-tokens]
  (let [^long precedence (operator->precedence operator-token 0)
        [right-expression rest-tokens] (parse-expression operator->precedence right-tokens precedence)
        operator-symbol (symbol (str operator-token))
        expression (list operator-symbol left-expression right-expression)]
    [expression rest-tokens]))

(defn- parse-expression [operator->precedence tokens ^long precedence]
  (loop [[left-expression [operator-token & right-tokens :as rest-tokens]] (parse-prefix operator->precedence tokens)]
    (if (< precedence ^long (operator->precedence operator-token 0))
      (recur (parse-infix operator->precedence left-expression operator-token right-tokens))
      [left-expression rest-tokens])))

(defn- parse [operator->precedence tokens]
  (first (parse-expression operator->precedence tokens 0)))

(defn- solve-task [input-text parse-fn]
  (->> input-text
       (string/split-lines)
       (map tokenize)
       (map parse-fn)
       (map eval)
       (reduce +)))

(defn- first-task
  "Rather than evaluating multiplication before addition, the operators have the
  same precedence, and are evaluated left-to-right regardless of the order in
  which they appear. Evaluate the expression on each line of the homework; what
  is the sum of the resulting values?"
  [input-text]
  (solve-task input-text (partial parse {\+ 1, \- 1, \* 1, \/ 1})))

(defn- second-task
  "Addition is evaluated before multiplication. What do you get if you add up
  the results of evaluating the homework problems using these new rules?"
  [input-text]
  (solve-task input-text (partial parse {\+ 2, \- 2, \* 1, \/ 1})))

;; -----------------------------------------------------------------------------

(assert (= 800602729153 (first-task input-text)))
(assert (= 92173009047076 (second-task input-text)))
