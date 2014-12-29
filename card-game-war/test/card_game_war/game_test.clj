(ns card-game-war.game-test
  (:require [clojure.test :refer :all]
            [card-game-war.game :refer :all]))

(deftest test-play-round
  (testing "the highest rank wins the cards in the round"
    (is (= :player-2 (play-round {:rank :two :suit :clubs} {:rank :three :suit :clubs})))
    (is (= :player-1 (play-round {:rank :four :suit :spades} {:rank :three :suit :spades}))))
  (testing "queens are higer rank than jacks"
    (is (= :player-1 (play-round {:rank :queen :suit :spades} {:rank :jack :suit :spades})))
    (is (= :player-2 (play-round {:rank :jack :suit :spades} {:rank :queen :suit :spades}))))
  (testing "kings are higer rank than queens"
    (is (= :player-1 (play-round {:rank :king :suit :spades} {:rank :queen :suit :spades})))
    (is (= :player-2 (play-round {:rank :queen :suit :spades} {:rank :king :suit :spades}))))
  (testing "aces are higer rank than kings"
    (is (= :player-1 (play-round {:rank :ace :suit :spades} {:rank :king :suit :spades})))
    (is (= :player-2 (play-round {:rank :king :suit :spades} {:rank :ace :suit :spades}))))
  (testing "if the ranks are equal, clubs beat spades"
    (is (= :player-1 (play-round {:rank :ten :suit :clubs} {:rank :ten :suit :spades})))
    (is (= :player-2 (play-round {:rank :ten :suit :spades} {:rank :ten :suit :clubs}))))
  (testing "if the ranks are equal, diamonds beat clubs"
    (is (= :player-1 (play-round {:rank :ten :suit :diamonds} {:rank :ten :suit :clubs})))
    (is (= :player-2 (play-round {:rank :ten :suit :clubs} {:rank :ten :suit :diamonds}))))
  (testing "if the ranks are equal, hearts beat diamonds"
    (is (= :player-1 (play-round {:rank :ten :suit :hearts} {:rank :ten :suit :diamonds})))
    (is (= :player-2 (play-round {:rank :ten :suit :diamonds} {:rank :ten :suit :hearts})))))

(deftest test-play-game
  (testing "the player loses when they run out of cards"
    (is (not (empty? (play-game))))
    (is (= [{:message "Player one wins the round!" :player-1-card {:rank :ace :suit :clubs} :player-2-card {:rank :two :suit :clubs} :round 1}
            {:message "Player one wins the game!" :player-1-card nil :player-2-card nil :round 2}]
           (play-game [{:rank :ace :suit :clubs}] [{:rank :two :suit :clubs}])))
    (is (= [{:message "Player one wins the round!" :player-1-card {:rank :three :suit :hearts} :player-2-card {:rank :three :suit :clubs} :round 1}
            {:message "Player one wins the round!" :player-1-card {:rank :three :suit :clubs} :player-2-card {:rank :two :suit :clubs} :round 2}
            {:message "Player one wins the game!" :player-1-card nil :player-2-card nil :round 3}]
           (play-game [{:rank :three :suit :hearts} {:rank :ace :suit :clubs}] [{:rank :three :suit :clubs} {:rank :two :suit :clubs}])))))

