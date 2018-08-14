(ns dinos.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :refer [run-server]]
            [clj-time.core :as t]
            [compojure.core :refer :all]
            [compojure.route :as route])
  (:gen-class))

(def free 0)
(def robot [1 0])
(def dino 2)

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
  (sequential? (nth (nth board y) x)))

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

(defn get-item 
  "returns item on given position"
  [board [x y]]
  (nth (nth board y) x))

(defn prettify
  "converts a board row to a string with pretty symbols"
  [row]
  (str (apply str (map #(if (= % 0) "#" 
                     (if (= % 2) "D" "R")) row)) "<br>"))

(defn pretty-board
  "prettify board for better representation"
  [board]
  (apply str (map prettify board)))


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
    (let [cur (get-item board [x y])
          dir (map f cur)
          pos (move [x y] dir)]
      (if (can-move? board pos)
        (place-in-board (erase-position board [x y]) pos cur)
      board))
    board))

(defn fwd-move
  [board [x y]]
  (do-move board [x y] identity))
  
(defn rev-move
  [board [x y]]
  (do-move board [x y] -))

(defn turn-right
  [dir]
  (case dir
    [0 -1] right-vec
    [1 0] down-vec
    [0 1] left-vec
    [-1 0] up-vec))

(defn turn-left
  [dir]
  (case dir
    [0 -1] left-vec
    [-1 0] down-vec
    [0 1] right-vec
    [1 0] up-vec))

(defn rotate
  [board [x y] f]
  (if (robot? board [x y])
    (let [pos (get-item board [x y])]
      (place-in-board board [x y] (f pos)))
    board))

(defn rotate-right
  [board [x y]]
  (rotate board [x y] turn-right))

(defn rotate-left
  [board [x y]]
  (rotate board [x y] turn-left))

;(defn move-up
;  "returns a new board with the robot above it's current position"
;  [board [x y]]
;  (do-move board [x y] go-up))
;
;(defn move-down
;  "returns a new board with the robot below it's current position"
;  [board [x y]]
;  (do-move board [x y] go-down))
;
;(defn move-right
;  "returns a new board with the robot going to the right of it's current position"
;  [board [x y]]
;  (do-move board [x y] go-right))
;
;(defn move-left
;  "returns a new board with the robot going to the left of it's current position"
;  [board [x y]]
;  (do-move board [x y] go-left))
;
(defn do-attack
  "attacks from given position or returns the board if attack is not valid"
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

;;;;
;; board state management
;;;;

(def board-dim 50)

(def board-state (atom (new-board board-dim)))

(defn reset-board-state
  "resets board state to default value"
  [& args]
  (reset! board-state (new-board board-dim)))

(defn get-state
  "returns the current state of the board"
  [& args]
  @board-state)

(defn change-state
  "receives a board function and change state to the return of this function"
  [f args]
  (swap! board-state f args))


;;;;
;; API logic
;;;;

(defn show-board-state
  []
  (let [response {:status 200
                  :headers {"Content-Type" "text/html"}
                  :body (str "<script>document.write(" (json/write-str (pretty-board (get-state))) ")</script>")}]
    response))

(defn a-handler
  [req]
  ;(do (println (json/read-str (slurp (:body req))))
  (let [body (json/read-str (slurp (:body req)) :key-fn keyword)
        response {:status 200
                  :body (json/write-str body)}]
    (println (:a body))
    response))

(defn place-dino-handler
  [req]
  (let [body (json/read-str (slurp (:body req)) :key-fn keyword)
        response {:status 200}]
    (change-state place-dino [(:x body) (:y body)])
    response))


(defroutes app
  (GET "/" [] "<h1>Welcome</h1>")
  (GET "/show-state" [] (show-board-state))
  (POST "/test-req" [] a-handler)
  (POST "/place-dino" [] place-dino-handler)
  (route/not-found "<h1>Page not found</h1>"))


(defn -main [& args]
    (run-server app {:port 8080})
      (println "Server started on port 8080"))

