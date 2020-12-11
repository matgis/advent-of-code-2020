(ns day11.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day11/input")))

(defn- seat [seats [^long x ^long y]]
  (let [row (get seats y)
        col (get row x)]
    col))

(def ^:private available-seat \L)
(def ^:private occupied-seat \#)
(def ^:private empty-space \.)
(def ^:private occupied-seat? (comp (partial = occupied-seat) seat))

(defn- update-seat [^long occupied-limit considered-positions-fn seats position]
  (let [seat (seat seats position)
        occupied-adjacent-seats (filter (partial occupied-seat? seats)
                                        (considered-positions-fn seats position))]
    (cond
      (and (= available-seat seat)
           (empty? occupied-adjacent-seats))
      occupied-seat

      (and (= occupied-seat seat)
           (<= occupied-limit
               ^long (bounded-count (inc occupied-limit)
                                    occupied-adjacent-seats)))
      available-seat

      :else
      seat)))

(defn- simulate-once [^long occupied-limit considered-positions-fn seats]
  (let [row-count (count seats)
        col-count (count (first seats))]
    (mapv (fn [^long row]
            (->> (range col-count)
                 (map (fn [^long col]
                        (update-seat occupied-limit considered-positions-fn seats [col row])))
                 (string/join)))
          (range row-count))))

(defn- simulate-until-stable [^long occupied-limit considered-positions-fn seats]
  (->> seats
       (iterate (partial simulate-once occupied-limit considered-positions-fn))
       (reduce (fn [prev-iteration iteration]
                 (if (= prev-iteration iteration)
                   (reduced iteration)
                   iteration)))))

(defn- count-occupied-seats [seats]
  (count (mapcat (partial filter #{occupied-seat}) seats)))

(defn- perform-task [^long occupied-limit considered-positions-fn input-text]
  (->> input-text
       (string/split-lines)
       (simulate-until-stable occupied-limit considered-positions-fn)
       (count-occupied-seats)))

(defn- make-out-of-bounds-fn [seats]
  (let [max-row (dec (count seats))
        max-col (dec (count (first seats)))]
    (fn [^long sx ^long sy]
      (or (neg? sx)
          (neg? sy)
          (< max-row sy)
          (< max-col sx)))))

(def ^:private surrounding-offsets (vector-of :long -1 0 1))

(defn- adjacent-positions [seats [^long x ^long y]]
  (let [out-of-bounds? (make-out-of-bounds-fn seats)]
    (for [sx (map + (repeat 3 x) surrounding-offsets)
          sy (map + (repeat 3 y) surrounding-offsets)
          :when (and (not (and (= x sx) (= y sy)))
                     (not (out-of-bounds? sx sy)))]
      [sx sy])))

(defn- first-task
  "Simulate peoples choice of seat.
  - If a seat is empty (L) and there are no occupied seats adjacent to it, the
    seat becomes occupied.
  - If a seat is occupied (#) and four or more seats adjacent to it are also
    occupied, the seat becomes empty.
  - Otherwise, the seat's state does not change.
  Once people stop moving around, how many seats end up occupied?"
  [input-text]
  (perform-task 4 adjacent-positions input-text))

(defn- line-of-sight-positions [seat-fn out-of-bounds-fn [^long x ^long y] [^long dx ^long dy :as delta]]
  (let [checked-x (+ x dx)
        checked-y (+ y dy)]
    (when-not (out-of-bounds-fn checked-x checked-y)
      (let [checked-position [(+ x dx) (+ y dy)]
            checked-seat (seat-fn checked-position)]
        (if (not= empty-space checked-seat)
          [checked-position]
          (line-of-sight-positions seat-fn out-of-bounds-fn checked-position delta))))))

(defn- seen-positions [seats position]
  (let [seat (partial seat seats)
        out-of-bounds? (make-out-of-bounds-fn seats)]
    (mapcat (partial line-of-sight-positions seat out-of-bounds? position)
            [[-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0]])))

(defn- second-task
  "How many seats end up occupied if we change the rules to only consider
  line-of-sight seats and allow 5 or more occupied seats?"
  [input-text]
  (perform-task 5 seen-positions input-text))
