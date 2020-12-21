(ns day21.solution
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day21/input")))

(defn- parse-food [line]
  (let [words (map keyword (re-seq #"\w+" line))
        [ingredients [_ & allergens]] (split-with #(not= :contains %) words)]
    {:ingredients (set ingredients)
     :allergens (set allergens)}))

(defn- parse-foods [input-text]
  (mapv parse-food (string/split-lines input-text)))

(defn- process-foods [foods]
  (let [all-allergens (into #{} (mapcat :allergens) foods)
        all-ingredients (into #{} (mapcat :ingredients) foods)]
    {:all-allergens all-allergens
     :all-ingredients all-ingredients
     :allergen-candidates (reduce (fn [ingredients-by-allergen {:keys [allergens ingredients]}]
                                    (reduce (fn [ingredients-by-allergen allergen]
                                              (update ingredients-by-allergen allergen set/intersection ingredients))
                                            ingredients-by-allergen
                                            allergens))
                                  (zipmap all-allergens
                                          (repeat all-ingredients))
                                  foods)}))

(defn- first-task
  "Determine which ingredients cannot possibly contain any of the allergens in
  your list. How many times do any of those ingredients appear?"
  [input-text]
  (let [foods (parse-foods input-text)
        {:keys [all-ingredients allergen-candidates]} (process-foods foods)
        safe-ingredients (set/difference all-ingredients
                                         (reduce set/union
                                                 (vals allergen-candidates)))]
    (transduce (comp (map :ingredients)
                     (map (partial set/intersection safe-ingredients))
                     (map count))
               +
               foods)))

(defn- only [coll]
  (when (= 1 (count coll))
    (first coll)))

(defn- map-vals
  ([f] (map (juxt key (comp f val))))
  ([f coll] (into {} (map-vals f) coll)))

(defn- solve-allergens [allergen-candidates]
  (if (every? (comp only val) allergen-candidates)
    (map-vals first allergen-candidates)
    (let [solved-ingredients (into #{} (keep only) (vals allergen-candidates))]
      (recur (into {}
                   (map (fn [[allergen candidate-ingredients :as entry]]
                          (if (= 1 (count candidate-ingredients))
                            entry
                            [allergen (set/difference candidate-ingredients
                                                      solved-ingredients)])))
                   allergen-candidates)))))

(defn- second-task
  "Arrange the allergenic ingredients alphabetically by their allergen and
  separate them by commas to produce your canonical dangerous ingredient list."
  [input-text]
  (->> input-text
       (parse-foods)
       (process-foods)
       (:allergen-candidates)
       (solve-allergens)
       (into (sorted-map))
       (vals)
       (map name)
       (string/join ",")))

;; -----------------------------------------------------------------------------

(assert (= 2211 (first-task input-text)))
(assert (= "vv,nlxsmb,rnbhjk,bvnkk,ttxvphb,qmkz,trmzkcfg,jpvz" (second-task input-text)))
