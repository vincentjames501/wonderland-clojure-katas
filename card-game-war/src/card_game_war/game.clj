(ns card-game-war.game
  "This kata is a version of the classic card game War.

   The rules of this card game are quite simple.

   There are two players.
   The cards are all dealt equally to each player.
   Each round, player 1 lays a card down face up at the same time that player 2 lays a card down face up. Whoever has
   the highest value card, wins both round and takes both cards.
   The winning cards are added to the bottom of the winners deck.
   Aces are high.
   If both cards are of equal value, then the winner is decided upon by the highest suit. The suits ranks in order of
   ascending value are spades, clubs, diamonds, and hearts.
   The player that runs out of cards loses.")

(def ^:private suits
  "The suit with their corresponding values."
  {:spades 1 :clubs 2 :diamonds 3 :hearts 4})

(def ^:private ranks
  "The possible ranks with their corresponding values."
  (zipmap [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king :ace] (range 2 15)))

(def ^:private cards
  "All possible card combinations represented as a map containing two keys: :rank and :suit.
   e.g. {:rank :two :suit :spade}"
  (for [suit suits rank ranks]
    {:suit (key suit) :rank (key rank)}))

(defn- round-details
  "Builds the round details for a given round of war."
  [round-number message & [player-1-card player-2-card]]
  {:round round-number :message message :player-1-card player-1-card :player-2-card player-2-card})

(defn play-round
  "Compares the two player cards and returns the winner (either :player-1 or :player-2)."
  [player-1-card player-2-card]
  (let [player-1-rank-value (get ranks (:rank player-1-card))
        player-2-rank-value (get ranks (:rank player-2-card))
        player-1-suit-value (get suits (:suit player-1-card))
        player-2-suit-value (get suits (:suit player-2-card))]
    (cond
      (> player-1-rank-value player-2-rank-value)
      :player-1
      (> player-2-rank-value player-1-rank-value)
      :player-2
      (> player-1-suit-value player-2-suit-value)
      :player-1
      (> player-2-suit-value player-1-suit-value)
      :player-2
      :else
      :player-1)))

(defn play-game
  "Plays the game of war with either a shuffled deck or predefined cards."
  ([]
    (let [shuffled-deck (shuffle cards)
          [player-1-cards player-2-cards] (split-at 26 shuffled-deck)]
      (play-game player-1-cards player-2-cards)))
  ([initial-player-1-cards initial-player-2-cards]
    (loop [player-1-cards initial-player-1-cards
           player-2-cards initial-player-2-cards
           round-number 1
           rounds []]
      (cond
        (empty? player-1-cards)
        (conj rounds (round-details round-number "Player two wins the game!"))

        (empty? player-2-cards)
        (conj rounds (round-details round-number "Player one wins the game!"))

        :else
        (let [player-1-card (first player-1-cards)
              player-2-card (first player-2-cards)
              winning-player (play-round player-1-card player-2-card)]
          (if (= winning-player :player-1)
            (recur (conj (rest player-1-cards) player-1-card player-2-card)
                   (rest player-2-cards)
                   (inc round-number)
                   (conj rounds (round-details round-number "Player one wins the round!" player-1-card player-2-card)))
            (recur (rest player-1-cards)
                   (conj (rest player-2-cards) player-1-card player-2-card)
                   (inc round-number)
                   (conj rounds (round-details round-number "Player two wins the round!" player-1-card player-2-card)))))))))
