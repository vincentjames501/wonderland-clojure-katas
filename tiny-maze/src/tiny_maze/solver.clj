(ns tiny-maze.solver
  "Alice found herself very tiny and wandering around Wonderland.  Even
   the grass around her seemed like a maze.

   This is a tiny maze solver.

   A maze is represented by a matrix

   [[:S 0 1]
    [1  0 1]
    [1  0 :E]]

   - S : start of the maze
   - E : end of the maze
   - 1 : This is a wall that you cannot pass through
   - 0 : A free space that you can move through.

   The goal is the get to the end of the maze.  A solved maze will have a
   :x in the start, the path, and the end of the maze, like this.

   [[:x :x 1]
    [1  :x 1]
    [1  :x :x]]")

(defn- row-count
  "Returns the row count of the maze.
   (row-count [[1] [2] [3]])
   => 3"
  [maze]
  (count maze))

(defn- col-count
  "Returns the column count of the maze.
   (col-count [[1 2 3]])
   => 3"
  [maze]
  (count (first maze)))

(defn- start-position
  "Finds the start position in the maze.
   (start-position [[:S 0 1] [0 0 1] [1 0 :E]])
   => [0 0]"
  [maze]
  (let [index (.indexOf (flatten maze) :S)]
    [(unchecked-divide-int index (row-count maze)) (mod index (col-count maze))]))

(defn- in-bounds?
  "Checks to see if the passed position is in the bounds of the row and columns of the maze.
   (in-bounds? 3 3 [4 0])
   => false
   (in-bounds? 3 3 [1 1])
   => true"
  [rows cols [x y]]
  (not (or (< x 0) (< y 0) (>= x rows) (>= y cols))))

(defn- valid-move?
  "Checks to see if the position is a valid position given a move. Valid positions are only those that are in bounds and
   have either a 0 or :E in their spot.
   (valid-move? [[:x :x 1] [0 0 1] [1 0 :E]] [1 2])
   => false
   (valid-move? [[:x :x 1] [0 0 1] [1 0 :E]] [1 1])
   => true"
  [move pos]
  (let [value (get-in move pos)]
    (or (= value 0) (= value :E))))

(defn- next-moves
  "Returns only the next valid positions given a maze and a current position.
  (next-moves [0 0] [[:x 0 1] [0 0 1] [1 0 :E]])
  => ([1 0] [0 1])"
  [[x y] maze]
  (let [rows (row-count maze)
        cols (col-count maze)
        positions [[(+ x 1) y] [x (+ y 1)] [x (- y 1)] [(- x 1) y]]]
    (filter #(and (in-bounds? rows cols %) (valid-move? maze %)) positions)))

(defn- solved?
  "Checks to see if the maze is solved by determining if there is an :E in the maze.
   (solved? [[:x :x 1] [1 :x 1] [1 :x :x]])
   => true
   (solved? [[:x :x 1] [1 :x 1] [1 :x :E]])
   => false"
  [maze]
  (= -1 (.indexOf (flatten maze) :E)))

(defn- solutions
  "Finds all possible solutions to the maze."
  [position moves]
  (let [current-move (last moves)
        next-moves (next-moves position current-move)]
    (if-not (seq next-moves)
      (when (solved? current-move)
        [moves])
      (mapcat #(solutions % (conj moves (assoc-in current-move % :x))) next-moves))))

(defn solve-maze
  "Solves the maze and returns the shortest solution."
  [maze]
  (let [start-position (start-position maze)
        initial-moves [(assoc-in maze start-position :x)]]
    (->> (solutions start-position initial-moves)           ; Find all the solutions
         (sort-by count)                                    ; Sort by the smallest solutions first
         first                                              ; Get the shortest solution
         last)))                                            ; Get the final state
