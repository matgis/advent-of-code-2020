(ns day08.solution
  (:require [clojure.java.io :as io]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day08/input")))

(defn- parse-instructions [input-text]
  (->> input-text
       (re-seq #"(.+?) ([+-]\d+)")
       (mapv (fn [[_ opcode arg]]
               [(keyword opcode) (Long/parseLong arg)]))))

(defn- process-acc [{:keys [^long acc ^long pos] :as _state} ^long arg]
  {:acc (+ acc arg)
   :pos (inc pos)})

(defn- process-jmp [state ^long arg]
  (update state :pos + arg))

(defn- process-nop [state]
  (update state :pos inc))

(defn- process-instruction [state [opcode arg]]
  (case opcode
    :acc (process-acc state arg)
    :jmp (process-jmp state arg)
    :nop (process-nop state)))

(defn- first-task
  "The moment the program tries to run any instruction a second time, you know
  it will never terminate. Immediately before any instruction is executed a
  second time, what value is in the accumulator?"
  [input-text]
  (let [instructions (parse-instructions input-text)]
    (loop [{:keys [pos] :as state} {:acc 0 :pos 0}
           seen-pos #{}]
      (if (contains? seen-pos pos)
        (:acc state)
        (recur (process-instruction state (instructions pos))
               (conj seen-pos pos))))))

(defn- patch-instruction [[opcode arg :as instruction]]
  (case opcode
    :nop [:jmp arg]
    :jmp [:nop arg]
    instruction))

(defn- working-program
  "Patch the supplied instructions at the specified position, and return a
  working program if the resulting instructions constitute a working program.
  Otherwise, return nil."
  [instructions patched-instruction-pos]
  (let [unpatched-instruction (instructions patched-instruction-pos)
        patched-instruction (patch-instruction unpatched-instruction)]
    ;; We already know the program won't terminate unless we patch it, in which
    ;; case we return nil.
    (when (not= unpatched-instruction patched-instruction)
      (loop [seen-pos #{}
             {:keys [pos] :as state} {:acc 0 :pos 0}]
        (cond
          ;; If we're able to finish the program with this patch, we return the
          ;; patched instructions.
          (= (count instructions) pos)
          (assoc instructions patched-instruction-pos patched-instruction)

          ;; If we're retreading an already-visited position, we're in a
          ;; non-terminating loop. This is not a working program. Return nil.
          (contains? seen-pos pos)
          nil

          ;; Else we keep executing. When we encounter the specified instruction
          ;; position, we swap it for the patched instruction before running it.
          :else
          (recur (conj seen-pos pos)
                 (process-instruction state
                                      (if (= patched-instruction-pos pos)
                                        patched-instruction
                                        (instructions pos)))))))))

(defn- second-task
  "Fix the program so that it terminates normally by changing exactly one jmp
  (to nop) or nop (to jmp). What is the value of the accumulator after the
  program terminates?"
  [input-text]
  (let [instructions (parse-instructions input-text)
        patched-instructions (some (partial working-program instructions)
                                   (range (count instructions)))]
    (loop [{:keys [pos] :as state} {:acc 0 :pos 0}]
      (if (= (count patched-instructions) pos)
        (:acc state)
        (recur (process-instruction state (patched-instructions pos)))))))
