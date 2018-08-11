(ns dinos.core
  (:gen-class))

(def dino 2)
(def robot 1)
(def free 0)

;;;;
;; Board operations
;;;;

(defn zeros*
  "generates a lazy sequence with only zeros"
  []
  (map (constantly 0) (range)))

(def zeros (zeros*))

(defn new-board
  "creates an empty board with dim x dim dimensions"
  ([dim cur]
   (if (= cur 0) '()
     (cons (take dim zeros) (new-board dim (dec cur)))))
  ([dim]
   (new-board dim dim)))

(defn place-in-row
  "receives a list and returns a new list with a modified element"
  [l pos value]
  (let [newl (take pos l)
        newl (concat newl (list value))
        newl (concat newl (drop (inc pos) l))]
    newl))

(defn place-in-board
  "receives a board and returns a new board with a modified element"
  [board [x y] value]
  (place-in-row board y (place-in-row (nth board y) x value)))

(defn place-dino
  "places a dino on the given position of the board"
  [board [x y]]
  (place-in-board board [x y] dino))

(defn place-robot
  "places a robot on the given position of the board"
  [board [x y]]
  (place-in-board board [x y] robot))

(defn erase-position
  "frees the given position of the board"
  [board [x y]]
  (place-in-board board [x y] free))

(defn item?
  "verifies if given item is on the given position"
  [board [x y] value]
  (= (nth (nth board y) x) value))

(defn robot?
  "verifies if there's a robot on given position"
  [board [x y]]
  (item? board [x y] robot))

(defn dino?
  "verifies if there's a dino on given position"
  [board [x y]]
  (item? board [x y] dino))

(defn free?
  "verifies if given position is free"
  [board [x y]]
  (item? board [x y] free))

(defn valid?
  "returns true if (x,y) is inside the board"
  [board [x y]]
  (let [dim (count board)]
    (and (>= x 0) (< x dim) (>= y 0) (< y dim))))

;;;;
;; robots' movement operations
;;;;

(def up-vec [0 -1])
(def down-vec [0 1])
(def right-vec [1 0])
(def left-vec [-1 0])

(defn move
  "returns new position based on given position and given direction"
  [[x y] dir]
  (map + [x y] dir))

(defn go-up
  [[x y]]
  (move [x y] up-vec))

(defn go-down
  [[x y]]
  (move [x y] down-vec))

(defn go-right
  [[x y]]
  (move [x y] right-vec))

(defn go-left
  [[x y]]
  (move [x y] left-vec))

(defn can-move?
  "returns true if robot can move to the given position"
  [board [x y]]
  (and (valid? board [x y]) (free? board [x y])))

(defn do-move
  "returns a new board with robot's position indicated by f or the board if move is not valid"
  [board [x y] f]
  (if (robot? board [x y])
    (let [pos (f [x y])]
      (if (can-move? board pos)
        (place-robot (erase-position board [x y]) pos)
    board))
  board))

(defn move-up
  "returns a new board with the robot above it's current position"
  [board [x y]]
  (do-move board [x y] go-up))

(defn move-down
  "returns a new board with the robot below it's current position"
  [board [x y]]
  (do-move board [x y] go-down))

(defn move-right
  "returns a new board with the robot going to the right of it's current position"
  [board [x y]]
  (do-move board [x y] go-right))

(defn move-left
  "returns a new board with the robot going to the left of it's current position"
  [board [x y]]
  (do-move board [x y] go-left))

(defn do-attack
  "removes dino of given position or returns the board if attack is not valid"
  [board [x y]]
  (if (and (valid? board [x y]) (dino? board [x y]))
    (erase-position board [x y])
    board))

(defn attack
  "makes robot from given position attack its adjacent positions"
  [board [x y]]
  (if (robot? board [x y])
    (let [left (go-left [x y])
          right (go-right [x y])
          down (go-down [x y])
          up (go-up [x y])]
      (reduce do-attack board [left right down up]))
    board))

;;; other

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
