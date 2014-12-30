(ns wonderland-number.finder
  "Wonderland is a strange place.  There is a wonderland number that is
   also quite strange.

   You must find a way to generate this wonderland number.

   - It has six digits
   - It you multiply it by 2,3,4,5, or 6, the resulting number has all
     the same digits in at as the original number.  The only difference
     is the position that they are in.")

(def ^:private wonderland-range
  "The wonderland range to search for the number."
  (range 100000 1000000))

(defn- digits
  "Given a number, returns a set of the digits.
   (digits \"123123\")
   => #{\\1 \\2 \\3}"
  [number]
  (set (str number)))

(defn- has-same-digits?
  "Checks to see if the two numbers have the same digits.
   (has-same-digits? 123123 112233)
   => true
   (has-same-digits? 456456 123123)
   => false"
  [number1 number2]
  (= (digits number1) (digits number2)))

(defn- wonderland-number?
  "Checks to see if the number is a wonderland number by making sure of the following:
   - It has six digits
   - It you multiply it by 2,3,4,5, or 6, the resulting number has all
     the same digits in at as the original number.  The only difference
     is the position that they are in."
  [number]
  (and (= 6 (count (str number)))
       (every? #(has-same-digits? number (* % number)) (range 2 7))))

(defn wonderland-number
  "Finds a wonderland number in the wonderland range."
  []
  (->> wonderland-range
       (filter wonderland-number?)
       first))
