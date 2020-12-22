(ns day22.solution
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (clojure.lang PersistentQueue)
           (java.io Writer)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private input-text
  (slurp (io/resource "day22/input")))

(defn- queue
  (^PersistentQueue [] PersistentQueue/EMPTY)
  (^PersistentQueue [& items] (into PersistentQueue/EMPTY items)))

(defn- subqueue
  (^PersistentQueue [q ^long start]
   (apply queue (drop start q)))
  (^PersistentQueue [q ^long start ^long end]
   (apply queue (take (- end start) (drop start q)))))

(defmethod print-method PersistentQueue [queue ^Writer writer]
  (.write writer ^String (or (some-> queue seq str) "()")))

(defn- parse-long
  ^long [str]
  (Long/parseLong str))

(defn- parse-player-decks [input-text]
  (let [[[_ & player-one-lines] _ [_ & player-two-lines]] (partition-by empty? (string/split-lines input-text))]
    [(into (queue) (map parse-long) player-one-lines)
     (into (queue) (map parse-long) player-two-lines)]))

(defn- max-index [coll]
  (first
    (reduce-kv (fn [[_max-index max-value :as prev-max] index value]
                 (if (or (nil? max-value)
                         (>= ^long value ^long max-value))
                   [index value]
                   prev-max))
               [nil nil]
               coll)))

(defn- score-deck
  ^long [deck]
  (reduce + (map * deck (range (count deck) 0 -1))))

(defn- solve-task [input-text play-game-fn]
  (let [decks (parse-player-decks input-text)
        winning-player-deck (second (play-game-fn decks))]
    (score-deck winning-player-deck)))

(def ^:private sort-descending (partial sort (comparator >)))

(defn- play-simple-game [[one-deck two-deck :as decks]]
  (cond
    (empty? one-deck)
    [1 two-deck] ; Player two wins.

    (empty? two-deck)
    [0 one-deck] ; Player one wins.

    :else
    (let [cards (mapv peek decks)
          winning-player-index (max-index cards)
          won-cards (sort-descending cards)
          decks (update (mapv pop decks) winning-player-index into won-cards)]
      (recur decks))))

(defn- first-task
  "Play the small crab in a game of Combat using the two decks you just dealt.
  What is the winning player's score?"
  [input-text]
  (solve-task input-text play-simple-game))

(defn- play-recursive-game
  ([decks]
   (play-recursive-game decks #{}))
  ([[one-deck two-deck :as decks] previous-rounds]
   (cond
     (contains? previous-rounds decks)
     [0 one-deck] ; Recursion loop. Player one wins by default.

     (empty? one-deck)
     [1 two-deck] ; Player two wins.

     (empty? two-deck)
     [0 one-deck] ; Player one wins.

     :else
     (let [previous-rounds (conj previous-rounds decks)
           [^long one-card ^long two-card :as cards] (mapv peek decks)
           [one-deck two-deck :as decks] (mapv pop decks)
           winning-player-index (long
                                  (if (and (<= one-card (count one-deck))
                                           (<= two-card (count two-deck)))
                                    (let [one-deck (subqueue one-deck 0 one-card)
                                          two-deck (subqueue two-deck 0 two-card)
                                          decks [one-deck two-deck]]
                                      (first (play-recursive-game decks previous-rounds)))
                                    (max-index cards)))
           won-cards (case winning-player-index
                       0 [one-card two-card]
                       1 [two-card one-card])
           decks (update decks winning-player-index into won-cards)]
       (recur decks previous-rounds)))))

(defn- second-task
  "Play the small crab in a game of Recursive Combat using the same two decks as
  before. What is the winning player's score?"
  [input-text]
  (solve-task input-text play-recursive-game))

;; -----------------------------------------------------------------------------

(assert (= 33098 (first-task input-text)))
(assert (= 35055 (second-task input-text)))
