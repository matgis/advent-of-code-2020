(ns day14.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day14/input")))

(def ^:private instruction-regex #"mask = ([01X]+)|mem\[(\d+)\] = (\d+)")

(defn- parse-instructions [input-text]
  (->> input-text
       (re-seq instruction-regex)
       (mapv (fn [[full-match :as match]]
               (cond
                 (string/starts-with? full-match "mask")
                 (let [[mask-str] (drop 1 match)]
                   [:set-mask mask-str])

                 (string/starts-with? full-match "mem")
                 (let [[address-str value-str] (drop 2 match)]
                   [:set-mem (Long/parseLong address-str) (Long/parseLong value-str)]))))))

(def ^:private initial-state {:mem {} :mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"})

(defn- first-task
  "The mask affects the value. What is the sum of all values left in memory
  after the program completes?"
  [input-text]
  (letfn [(apply-mask-to-value [mask ^long value]
            (reduce (fn [masked-value [^long index letter]]
                      (case letter
                        \X masked-value
                        \0 (bit-clear masked-value (- 35 index))
                        \1 (bit-set masked-value (- 35 index))))
                    value
                    (map-indexed vector mask)))
          (process-instruction [state [opcode arg1 arg2]]
            (case opcode
              :set-mem (assoc-in state [:mem arg1] (apply-mask-to-value (:mask state) arg2))
              :set-mask (assoc state :mask arg1)))
          (run-program [instructions]
            (reduce process-instruction initial-state instructions))]
    (->> input-text
         (parse-instructions)
         (run-program)
         (:mem)
         (vals)
         (reduce +))))

(defn- second-task
  "The mask affects the mutated addresses. What is the sum of all values left in
  memory after the program completes?"
  [input-text]
  (letfn [(masked-address
            ^long [mask ^long address]
            (reduce (fn [masked-address [^long index letter]]
                      (if (= \1 letter)
                        (bit-set masked-address (- (dec (count mask)) index))
                        masked-address))
                    address
                    (map-indexed vector mask)))
          (floating-bit-indices [mask]
            (sequence (comp (map (fn [^long index letter]
                                   (when (= \X letter)
                                     (- (dec (count mask)) index))))
                            (filter some?))
                      (range)
                      mask))
          (floating-bit-addresses [mask ^long address]
            (reduce (fn [addresses ^long bit-index]
                      (into addresses
                            (mapcat (fn [address]
                                      [(bit-clear address bit-index)
                                       (bit-set address bit-index)]))
                            addresses))
                    #{(masked-address mask address)}
                    (floating-bit-indices mask)))
          (assoc-many [coll keys value]
            (reduce #(assoc %1 %2 value) coll keys))
          (process-instruction [state [opcode arg1 arg2]]
            (case opcode
              :set-mask (assoc state :mask arg1)
              :set-mem (update state :mem assoc-many (floating-bit-addresses (:mask state) arg1) arg2)))
          (run-program [instructions]
            (reduce process-instruction initial-state instructions))]
    (->> input-text
         (parse-instructions)
         (run-program)
         (:mem)
         (vals)
         (reduce +))))

;; -----------------------------------------------------------------------------

(assert (= 7817357407588 (first-task input-text)))
(assert (= 4335927555692 (second-task input-text)))
