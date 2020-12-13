(ns day13.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day13/input")))

(defn- parse-long
  ^long [str]
  (Long/parseLong str))

(defn- find-earliest-departure [^long earliest-departure bus-ids]
  (letfn [(next-departure [^long bus-id]
            (* bus-id (inc (quot earliest-departure bus-id))))]
    (->> bus-ids
         (map (juxt next-departure identity))
         (apply min-key first))))

(defn- first-task
  "What is the ID of the earliest bus you can take to the airport multiplied by
  the number of minutes you'll need to wait for that bus?"
  ^long [input-text]
  (let [[earliest-departure-str bus-ids-str] (string/split-lines input-text)
        earliest-departure (parse-long earliest-departure-str)
        bus-ids (map parse-long (re-seq #"\d+" bus-ids-str))
        [^long departure ^long bus-id] (find-earliest-departure earliest-departure bus-ids)
        wait (- departure earliest-departure)]
    (* wait bus-id)))

(defn- extended-greatest-common-divisor [^long a ^long b]
  (if (zero? a)
    [b 0 1]
    (let [[^long gcd ^long s ^long t] (extended-greatest-common-divisor (mod b a) a)]
      [gcd (- t (* s (quot b a))) s])))

(defn- modular-multiplicative-inverse
  ^long [^long a ^long b]
  (let [[greatest-common-divisor bezout-coefficient] (extended-greatest-common-divisor a b)]
    (assert (= greatest-common-divisor 1))
    (mod bezout-coefficient b)))

(defn- solve-chinese-remainder-theorem
  ^long [remainders-and-mod-divs]
  (let [^long N (transduce (map second) * remainders-and-mod-divs)
        ^long x (transduce (map (fn ^long [[^long bi ^long ni]]
                                  (let [Ni (quot N ni)
                                        xi (modular-multiplicative-inverse Ni ni)]
                                    (* bi Ni xi))))
                           +
                           remainders-and-mod-divs)]
    (rem x N)))

(defn- second-task
  "What is the earliest timestamp such that all of the listed bus IDs depart at
  offsets matching their positions in the list?"
  ^long [input-text]
  (let [bus-ids-str (second (string/split-lines input-text))
        bus-ids (into (vector-of :long)
                      (map #(if (= "x" %) 0 (parse-long %)))
                      (string/split bus-ids-str #","))
        remainders-and-mod-divs (keep-indexed (fn [^long index ^long bus-id]
                                                (when-not (zero? bus-id)
                                                  (vector-of :long
                                                             (- (count bus-ids) index 1)
                                                             bus-id)))
                                              bus-ids)
        ^long last-offset-from-first (ffirst remainders-and-mod-divs)]
    (- (solve-chinese-remainder-theorem remainders-and-mod-divs)
       last-offset-from-first)))

;; -----------------------------------------------------------------------------

(assert (= 2845 (first-task input-text)))
(assert (= 487905974205117 (second-task input-text)))
