(ns doublets.solver
  "This Clojure Kata comes from Alice in Wonderland's author, Lewis Carroll. He came up with this word puzzle that he
   named Doublets.

   The puzzle is to take two words of the same length and find a way of linking the first word to the second word by only changing one letter at a time. At the end of the transformation, there will be a collections of words that show the beginning word being changed into the ending word, one letter at a time. All the word links must be in Lewis Carroll's own words:

   ... it is de rigueur that the links should be English words, such as might be used in good society.
   Also the word links should be words that are found in the dictionary. No proper nouns.

   Here are some examples.

   The Doublet of DOOR to LOCK is:

   door
   boor
   book
   look
   lock

   The Doublet of BANK to LOAN is:

   bank
   bonk
   book
   look
   loon
   loan"
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def ^:private a-z
  "Every characters from 'a' to 'z'."
  (map char (range 97 123)))

(def ^:private words
  "The dictionary words for the doublets puzzle."
  (->> "words.edn"
       (io/resource)
       (slurp)
       (edn/read-string)
       (into #{})))

(defn- replace-char-at-index
  "Helper function to replace a character at a given index.
   e.x
   (replace-char-at-index \"blah\" \\z 1)
   => \"bzah\""
  [word character index]
  (str (subs word 0 index) character (subs word (inc index))))

(defn- possible-next-words
  "Finds all of the possible next words that are in the dictionary, not equal to the current word, and have not already
   been used."
  [state]
  (let [word (last state)]
    (->>
      (reduce (fn [all-words character]
                (concat all-words (map (partial replace-char-at-index word character) (range (count word)))))
              []
              a-z)
      (filter #(and (not= word %)
                    (contains? words %)
                    (not (contains? (into #{} state) %)))))))

(defn- find-doublets
  "Finds all doublets recursively."
  [word1 word2 state]
  (let [next-words (possible-next-words state)]
    (if (some #(= word2 %) next-words)
      (conj state word2)
      (map #(flatten (find-doublets word1 word2 (conj state %))) next-words))))

(defn doublets
  "Finds the shortest doublet combination."
  ([word1 word2]
    (let [initial-state [word1]
          all-doublets (find-doublets word1 word2 initial-state)]
      (or (first (sort-by count all-doublets)) []))))
