(ns day19.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day19/input")))

(defn- parse-long
  ^long [str]
  (Long/parseLong str))

(def ^:private numbers-re #"\d+")

(defn- parse-numbers [numbers-str]
  (mapv parse-long (re-seq numbers-re numbers-str)))

(defn- parse-rule [rule-str]
  (if (string/starts-with? rule-str "\"")
    (subs rule-str 1 (dec (count rule-str)))
    (mapv parse-numbers (string/split rule-str #"\|"))))

(defn- parse-rule-entry [line]
  (let [[index-str rule-str] (string/split line #": ")
        rule-number (parse-long index-str)
        rule (parse-rule rule-str)]
    [rule-number rule]))

(defn- parse-rules [rules-str]
  (into {}
        (map parse-rule-entry)
        (string/split-lines rules-str)))

(defn- parse-input [input-text]
  (let [[rules-str messages-str] (string/split input-text #"\n{2}")]
    {:rules (parse-rules rules-str)
     :messages (string/split-lines messages-str)}))

(defn- valid-message? [message rules [rule-number & rest-rule-numbers]]
  (if (or (empty? message) (nil? rule-number))
    (and (empty? message) (nil? rule-number))
    (let [rule (rules rule-number)]
      (if (string? rule)
        (when (= (first rule) (first message))
          (recur (rest message) rules rest-rule-numbers))
        (some (partial valid-message? message rules)
              (map #(into % rest-rule-numbers) rule))))))

(defn- valid-messages [rules rule-number messages]
  (filter #(valid-message? % rules [rule-number]) messages))

(defn- first-task
  "How many messages completely match rule 0?"
  [input-text]
  (let [{:keys [rules messages]} (parse-input input-text)]
    (count (valid-messages rules 0 messages))))

(defn- second-task
  "After updating rules 8 and 11, how many messages completely match rule 0?"
  [input-text]
  (let [{:keys [rules messages]} (parse-input input-text)
        rules (assoc rules
                8 [[42] [42 8]]
                11 [[42 31] [42 11 31]]
                666 [[1 14] [1 666 14]])]
    (count (valid-messages rules 0 messages))))

;; -----------------------------------------------------------------------------

(assert (= 265 (first-task input-text)))
(assert (= 394 (second-task input-text)))
