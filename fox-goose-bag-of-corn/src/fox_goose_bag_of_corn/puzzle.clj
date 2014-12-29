(ns fox-goose-bag-of-corn.puzzle
  "Solves one of Lewis Carroll's favorite puzzles to ask children was the one about the Fox, Goose, and Bag of Corn. It
   has to do with getting them all safely across a river.

   The rules for this puzzle are:

   You must get the fox, goose, and bag of corn safely across the other side of the river
   You can only carry 1 item on the boat across with you.
   The fox cannot be left alone with the goose, (or it will be eaten).
   The goose cannot be left alone with the corn, (or it will be eaten).

   The starting position is you, the fox, the goose, and corn on one side of the river. The boat is empty. The other
   river bank is empty."
  (:require [clojure.set :refer [difference union]]))

(def ^:private initial-state
  "The initial state of the problem. The state will be represented as a vector of 'moves' composed of 'positions'."
  [[#{:fox :goose :corn :you} #{:boat} #{}]])

(def ^:private final-move
  "The final move which should be the final move in the state."
  [#{} #{:boat} #{:fox :goose :corn :you}])

(def ^:private moveable-items
  "The set of every item that can be moved."
  #{:fox :goose :corn :you})

(def ^:private invalid-positions
  "The set of all invalid positions."
  #{#{:fox :goose} #{:goose :corn}})

(defn- final-move?
  "Checks to see if the move is the final move."
  [move]
  (= final-move move))

(defn- valid-move?
  "Checks to see if the move is valid by making sure that no position contains an invalid position."
  [move]
  (not-any? (fn [position] (some #(= position %) invalid-positions)) move))

(defn- not-repeated-move?
  "We don't want to repeat moves, so make sure the last move's positions has never been seen before."
  [state]
  (not-any? #(= (last state) %) (butlast state)))

(defn- you-location
  "Finds the location of where 'you' are and returns either :first, :middle, or :last."
  [position]
  (cond
    (contains? (first position) :you)
    :first
    (contains? (second position) :you)
    :middle
    :default
    :last))

(defn- next-states
  "Given a state, return all possible next states by applying only valid and non repeated moves."
  [state]
  (let [current-move (last state)
        location (you-location current-move)]
    (->> (if (= location :middle)
           [(conj state [(union (first current-move) (difference (second current-move) #{:boat})) #{:boat} (last current-move)])
            (conj state [(first current-move) #{:boat} (union (last current-move) (difference (second current-move) #{:boat}))])]
           (map #(when (contains? (if (= location :first) (first current-move) (last current-move)) %)
                  (conj state [(if (= location :first)
                                 (difference (first current-move) (conj #{:you} %))
                                 (first current-move))
                               (conj #{:boat :you} %)
                               (if (= location :first)
                                 (last current-move)
                                 (difference (last current-move) (conj #{:you} %)))]))
                moveable-items))
         (filter #(and (not (nil? %))
                       (valid-move? (last %))
                       (not-repeated-move? %))))))

(defn- find-first-final-move
  "Looks through the states to find the first state that contains the final move."
  [states]
  (first (filter #(final-move? (last %)) states)))

(defn river-crossing-plan
  "Solves the puzzle."
  ([] (river-crossing-plan initial-state))
  ([state]
    (let [next-states (next-states state)]
      (or (find-first-final-move next-states)
          (find-first-final-move (map river-crossing-plan next-states))))))
