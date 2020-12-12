(ns day12.solution
  (:require [clojure.java.io :as io]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day12/input")))

(defn- parse-opcode [opcode-str]
  (case opcode-str
    "N" :north
    "S" :south
    "E" :east
    "W" :west
    "L" :left
    "R" :right
    "F" :forward))

(defn- parse-instructions [input-text]
  (into []
        (map (fn [[_ opcode arg]]
               [(parse-opcode opcode) (Long/parseLong arg)]))
        (re-seq #"([NSEWLRF])(\d+)" input-text)))

(defn- opposite-direction [direction]
  (case direction
    :south :north
    :west :east))

(defn- manhattan-distance [{:keys [^long east ^long north]}]
  (+ (Math/abs east)
     (Math/abs north)))

(defn- first-task
  "What is the manhattan distance of the ship from its starting position after all the actions have been performed?
  - Action N means to move north by the given value.
  - Action S means to move south by the given value.
  - Action E means to move east by the given value.
  - Action W means to move west by the given value.
  - Action L means to turn left the given number of degrees.
  - Action R means to turn right the given number of degrees.
  - Action F means to move forward by the given value in the direction the ship is currently facing."
  ^long [input-text]
  (letfn [(direction->degrees
            ^long [direction]
            (case direction
              :east 0
              :south 90
              :west 180
              :north 270))
          (degrees->direction [degrees]
            (case (long (mod degrees 360))
              0 :east
              90 :south
              180 :west
              270 :north))
          (move-ship [state direction ^long units]
            (case direction
              (:north :east) (update state direction + units)
              (:south :west) (update state (opposite-direction direction) - units)
              (:forward) (move-ship state (:direction state) units)))
          (rotate-ship [state rotation-direction ^long degrees]
            (let [apply-degrees (case rotation-direction :left - :right +)
                  rotate #(-> %1 direction->degrees (apply-degrees %2) degrees->direction)]
              (update state :direction rotate degrees)))
          (process-instruction [state [opcode arg]]
            (case opcode
              (:north :south :east :west :forward) (move-ship state opcode arg)
              (:left :right) (rotate-ship state opcode arg)))]
    (manhattan-distance
      (reduce process-instruction
              {:direction :east :east 0 :north 0}
              (parse-instructions input-text)))))

(defn- map-vals
  ([f] (map (juxt key (comp f val))))
  ([f coll] (into {} (map-vals f) coll)))

(defn- second-task
  "Almost all of the actions indicate how to move a waypoint which is relative to the ship's position:
  - Action N means to move the waypoint north by the given value.
  - Action S means to move the waypoint south by the given value.
  - Action E means to move the waypoint east by the given value.
  - Action W means to move the waypoint west by the given value.
  - Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
  - Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
  - Action F means to move forward to the waypoint a number of times equal to the given value.
  What is the manhattan distance of the ship from its starting position after all the actions have been performed?"
  ^long [input-text]
  (letfn [(rotate [{:keys [^long east ^long north] :as point} rotation-direction ^long degrees]
            (let [clamped-degrees (-> rotation-direction
                                      (case :left (- degrees) :right degrees)
                                      (mod 360)
                                      (long))]
              (case clamped-degrees
                0 point
                90 {:east north :north (- east)}
                180 {:east (- east) :north (- north)}
                270 {:east (- north) :north east})))
          (rotate-waypoint [state rotation-direction ^long degrees]
            (update state :waypoint rotate rotation-direction degrees))
          (move-waypoint [state direction ^long units]
            (case direction
              (:north :east) (update-in state [:waypoint direction] + units)
              (:south :west) (update-in state [:waypoint (opposite-direction direction)] - units)))
          (move-ship-towards-waypoint [state ^long times]
            (update state :ship (partial merge-with +) (map-vals (partial * times) (:waypoint state))))
          (process-instruction [state [opcode ^long arg]]
            (case opcode
              (:north :south :east :west) (move-waypoint state opcode arg)
              (:left :right) (rotate-waypoint state opcode arg)
              (:forward) (move-ship-towards-waypoint state arg)))]
    (->> input-text
         (parse-instructions)
         (reduce process-instruction
                 {:ship {:east 0, :north 0}
                  :waypoint {:east 10, :north 1}})
         (:ship)
         (manhattan-distance))))

;; -----

(assert (= 364 (first-task input-text)))
(assert (= 39518 (second-task input-text)))
