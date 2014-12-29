(ns alphabet-cipher.coder
  "Lewis Carroll published a cipher known as The Alphabet Cipher.

   This Alphabet Cipher involves alphabet substitution using a keyword.

   First you must make a substitution chart like this, where each row of the alphabet is rotated by one as each letter
   goes down the chart.

   Both parties need to decide on a secret keyword. This keyword is not written down anywhere, but memorized.

   To encode the message, first write down the message.

   meetmebythetree
   Then, write the keyword, (which in this case is scones), repeated as many times as necessary.

   sconessconessco
   meetmebythetree
   Now you can look up the column S in the table and follow it down until it meets the M row. The value at the
   intersection is the letter e. All the letters would be thus encoded.

   sconessconessco
   meetmebythetree
   egsgqwtahuiljgs
   The encoded message is now egsgqwtahuiljgs

   To decode, the person would use the secret keyword and do the opposite."
  (:require [clojure.string :refer [join]]))

(def ^:private a-z
  "Every characters from 'a' to 'z'."
  (map char (range 97 123)))

(def ^:private substitution-map
  "A substitution map for Alphabet Cipher."
  (reduce (fn [lookup [index char]]
            (assoc lookup char (zipmap a-z (take 26 (drop index (cycle a-z))))))
          {}
          (map-indexed vector a-z)))

(defn encode
  "Encodes the message using Alphabet Cipher given a keyword and message."
  [keyword message]
  (let [full-keyword (take (count message) (cycle keyword))]
    (->> (map vector full-keyword message)
         (map #(get-in substitution-map %))
         join)))

(defn decode
  "Decodes the message using Alphabet Cipher given a keyword and message."
  [keyword message]
  (let [full-keyword (take (count message) (cycle keyword))]
    (->> (map vector full-keyword message)
         (map (fn [[keyword-char message-char]]
                (->> (get substitution-map keyword-char)
                     (filter #(= (val %) message-char))
                     first
                     key)))
         join)))
