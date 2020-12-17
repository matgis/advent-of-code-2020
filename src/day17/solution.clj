(ns day17.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day17/input")))

(defn- parse-grid [input-text]
  (let [lines (string/split-lines input-text)
        row-count (count lines)
        col-count (count (first lines))
        min-bound (vector-of :int 0 0 0 0)
        max-bound (vector-of :int (dec col-count) (dec row-count) 0 0)
        active-coordinates (into #{}
                                 (mapcat (fn [^long row line]
                                           (keep (fn [^long col]
                                                   (when (= \# (nth line col))
                                                     (vector-of :int col row 0 0)))
                                                 (range col-count)))
                                         (range row-count)
                                         lines))]
    {:active-coordinates active-coordinates
     :min-bound min-bound
     :max-bound max-bound}))

(defn- min-coordinate [a b]
  (into (vector-of :int) (map min a b)))

(defn- max-coordinate [a b]
  (into (vector-of :int) (map max a b)))

(defn- activate-coordinate [grid coordinate]
  (-> grid
      (update :active-coordinates conj coordinate)
      (update :min-bound min-coordinate coordinate)
      (update :max-bound max-coordinate coordinate)))

(defn- deactivate-coordinate [grid coordinate]
  (update grid :active-coordinates disj coordinate))

(defn- simulate-once [tested-coordinates-fn neighbor-coordinates-fn {:keys [active-coordinates] :as grid}]
  (reduce (fn [new-grid tested-coordinate]
            (let [neighbor-coordinates (neighbor-coordinates-fn tested-coordinate)
                  active-neighbor-count (int (bounded-count 4 (filter active-coordinates neighbor-coordinates)))]
              (if (active-coordinates tested-coordinate)
                (case active-neighbor-count
                  (2 3) new-grid
                  (deactivate-coordinate new-grid tested-coordinate))
                (case active-neighbor-count
                  (3) (activate-coordinate new-grid tested-coordinate)
                  new-grid))))
          grid
          (tested-coordinates-fn grid)))

(defn- solve-task [input-text simulate-once-fn]
  (->> input-text
       (parse-grid)
       (iterate simulate-once-fn)
       (drop 6)
       (first)
       (:active-coordinates)
       (count)))

(defn- first-task
  "Starting with your given initial configuration, simulate six cycles in a
  three-dimensional grid. How many cubes are left in the active state after the
  sixth cycle?"
  [input-text]
  (letfn [(neighbor-coordinates-3d [[^int x ^int y ^int z]]
            (for [^int dx [-1 0 1]
                  ^int dy [-1 0 1]
                  ^int dz [-1 0 1]
                  :when (not (and (zero? dx) (zero? dy) (zero? dz)))]
              (vector-of :int (+ x dx) (+ y dy) (+ z dz) 0)))
          (tested-coordinates-3d [{[^int -x ^int -y ^int -z] :min-bound [^int +x ^int +y ^int +z] :max-bound}]
            (for [^int x (range (dec -x) (+ 2 +x))
                  ^int y (range (dec -y) (+ 2 +y))
                  ^int z (range (dec -z) (+ 2 +z))]
              (vector-of :int x y z 0)))]
    (solve-task input-text (partial simulate-once tested-coordinates-3d neighbor-coordinates-3d))))

(defn- second-task
  "Starting with your given initial configuration, simulate six cycles in a
  four-dimensional grid. How many hypercubes are left in the active state after
  the sixth cycle?"
  [input-text]
  (letfn [(neighbor-coordinates-4d [[^int x ^int y ^int z ^int w]]
            (for [^int dx [-1 0 1]
                  ^int dy [-1 0 1]
                  ^int dz [-1 0 1]
                  ^int dw [-1 0 1]
                  :when (not (and (zero? dx) (zero? dy) (zero? dz) (zero? dw)))]
              (vector-of :int (+ x dx) (+ y dy) (+ z dz) (+ w dw))))
          (tested-coordinates-4d [{[^int -x ^int -y ^int -z ^int -w] :min-bound [^int +x ^int +y ^int +z ^int +w] :max-bound}]
            (for [^int x (range (dec -x) (+ 2 +x))
                  ^int y (range (dec -y) (+ 2 +y))
                  ^int z (range (dec -z) (+ 2 +z))
                  ^int w (range (dec -w) (+ 2 +w))]
              (vector-of :int x y z w)))]
    (solve-task input-text (partial simulate-once tested-coordinates-4d neighbor-coordinates-4d))))

;; -----------------------------------------------------------------------------

(assert (= 395 (first-task input-text)))
(assert (= 2296 (second-task input-text)))
