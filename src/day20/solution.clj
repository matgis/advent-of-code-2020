(ns day20.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day20/input")))

(defn- parse-tile-number
  ^long [header]
  (Long/parseLong (re-find #"\d+" header)))

(defn- parse-edges [image-lines]
  {:top (first image-lines)
   :right (string/join (map last image-lines))
   :bottom (last image-lines)
   :left (string/join (map first image-lines))})

(def ^:private edge-opposites
  {:top :bottom
   :right :left
   :bottom :top
   :left :right})

(defn- crop-edges [image-lines]
  (into []
        (map (comp string/join next butlast))
        (next (butlast image-lines))))

(defn- parse-image [tile-number image-lines]
  {:tile-number tile-number
   :image (crop-edges image-lines)
   :edges (parse-edges image-lines)})

(def ^:private flip-image-horizontally (partial mapv string/reverse))
(def ^:private flip-image-vertically (comp vec reverse))

(defn- rotate-image [image-lines]
  (mapv (fn [^long col]
          (string/join (map (fn [line]
                              (nth line col))
                            (reverse image-lines))))
        (range (count (first image-lines)))))

(defn- image-permutations [image-lines]
  (sequence
    (mapcat (juxt identity
                  flip-image-horizontally
                  flip-image-vertically
                  (comp flip-image-horizontally
                        flip-image-vertically)))
    [image-lines (rotate-image image-lines)]))

(defn- parse-tiles [input-text]
  (let [partitioned (->> input-text
                         (string/split-lines)
                         (remove empty?)
                         (partition-by #(string/starts-with? % "Tile ")))]
    (map (fn [[header] tile-lines]
           [(parse-tile-number header) tile-lines])
         (take-nth 2 partitioned)
         (take-nth 2 (next partitioned)))))

(defn- make-image-permutations [parsed-tiles]
  (into []
        (mapcat (fn [[tile-number tile-lines]]
                  (map (partial parse-image tile-number)
                       (image-permutations tile-lines))))
        parsed-tiles))

(defn- find-index [pred coll]
  (reduce-kv (fn [_ index value]
               (when (pred value)
                 (reduced index)))
             nil
             coll))

(defn- adjacent-tile-index [tiles tile-index edge-kw]
  (let [opposite-edge-kw (edge-opposites edge-kw)
        tile (tiles tile-index)
        tile-number (:tile-number tile)
        edge (-> tile :edges edge-kw)]
    (find-index (fn [compared-tile]
                  (and (not= tile-number (:tile-number compared-tile))
                       (= edge (-> compared-tile :edges opposite-edge-kw))))
                tiles)))

(defn- adjacencies [tiles]
  (mapv (fn [tile-index]
          (into {}
                (map (juxt identity (partial adjacent-tile-index tiles tile-index)))
                (keys edge-opposites)))
        (range (count tiles))))

(defn- find-corner-tile-numbers [tiles adjacencies]
  (into #{}
        (keep-indexed (fn [tile-index edge-kw->tile-index]
                        (let [unmatched-edges (filter (comp nil? val) edge-kw->tile-index)
                              is-corner-tile (= 2 (count unmatched-edges))]
                          (when is-corner-tile
                            (:tile-number (tiles tile-index))))))
        adjacencies))

(defn- first-task
  "Assemble the tiles into an image. What do you get if you multiply together
  the IDs of the four corner tiles?"
  [input-text]
  (let [parsed-tiles (parse-tiles input-text)
        tiles (make-image-permutations parsed-tiles)
        adjacencies (adjacencies tiles)
        corner-tile-numbers (find-corner-tile-numbers tiles adjacencies)]
    (reduce * corner-tile-numbers)))

(defn- find-adjacent-tile-index [adjacencies edge-spec]
  (find-index (fn [edge-kw->tile-index]
                (every? (fn [[edge-kw tile-index]]
                          (= tile-index (edge-kw->tile-index edge-kw)))
                        edge-spec))
              adjacencies))

(defn- solve-tile-index-grid [^long grid-size adjacencies]
  (loop [grid-row 0
         grid-col 0
         grid (vec (repeat grid-size (vec (repeat grid-size nil))))]
    (cond
      (= grid-size grid-row)
      grid ; Done!

      (= grid-size grid-col)
      (recur (inc grid-row) 0 grid) ; Next row.

      :else
      (recur grid-row
             (inc grid-col)
             (assoc-in grid
                       [grid-row grid-col]
                       (find-adjacent-tile-index
                         adjacencies
                         {:top (get-in grid [(dec grid-row) grid-col])
                          :left (get-in grid [grid-row (dec grid-col)])}))))))

(defn- join-image [tile-grid]
  (into []
        (mapcat (fn [tile-row]
                  (map (fn [^long image-line-number]
                         (string/join
                           (map (fn [tile]
                                  (get-in tile [:image image-line-number]))
                                tile-row)))
                       (range 8))))
        tile-grid))

(defn- char-at [image-lines ^long x ^long y]
  (nth (nth image-lines y) x))

(def ^:private sea-monster
  (let [image-lines ["                  # "
                     "#    ##    ##    ###"
                     " #  #  #  #  #  #   "]]
    {:image-lines image-lines
     :width (count (first image-lines))
     :height (count image-lines)}))

(def ^:private sea-monster-points
  (into []
        (mapcat (fn [^long y]
                  (keep-indexed (fn [^long x char]
                                  (when (= \# char)
                                    (vector-of :long x y)))
                                (get-in sea-monster [:image-lines y]))))
        (range (:height sea-monster))))

(defn- has-sea-monster? [image-lines ^long x ^long y]
  (every? (fn [[^long x ^long y]]
            (= \# (char-at image-lines x y)))
          (map (partial mapv +)
               sea-monster-points
               (repeat (vector-of :long x y)))))

(defn- find-sea-monsters [image-lines]
  (let [image-width (count (first image-lines))
        image-height (count image-lines)]
    (for [^long x (range (- image-width ^long (:width sea-monster)))
          ^long y (range (- image-height ^long (:height sea-monster)))
          :when (has-sea-monster? image-lines x y)]
      (vector-of :long x y))))

(defn- count-waves
  ^long [image-lines]
  (count (filter (partial = \#)
                 (apply concat image-lines))))

(defn- max-by [key-fn coll]
  (loop [keys (map key-fn coll)
         values coll
         max-key nil
         max-value nil]
    (if-some [key (first keys)]
      (if (or (nil? max-key) (>= ^long key ^long max-key))
        (recur (next keys) (next values) key (first values))
        (recur (next keys) (next values) max-key max-value))
      max-value)))

(defn- second-task
  "Determine how rough the waters are in the sea monsters' habitat by counting
  the number of '#' characters that are not part of a sea monster."
  [input-text]
  (let [parsed-tiles (parse-tiles input-text)
        grid-size (long (Math/sqrt (count parsed-tiles)))
        tiles (make-image-permutations parsed-tiles)
        adjacencies (adjacencies tiles)
        tile-index-grid (solve-tile-index-grid grid-size adjacencies)
        tile-grid (mapv (partial mapv tiles) tile-index-grid)
        joined-image (join-image tile-grid)
        joined-image-permutations (image-permutations joined-image)
        found-sea-monsters (max-by count (map find-sea-monsters joined-image-permutations))]
    (- (count-waves joined-image)
       (* (count sea-monster-points)
          (count found-sea-monsters)))))

;; -----------------------------------------------------------------------------

(assert (= 17250897231301 (first-task input-text)))
(assert (= 1576 (second-task input-text)))
