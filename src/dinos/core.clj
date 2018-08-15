(ns dinos.core
  (:require [clojure.data.json :as json])
  (:require [org.httpkit.server :refer [run-server]]
            [compojure.core :refer :all]
            [compojure.route :as route])
  (:require [clojure.tools.logging :as log])
  (:gen-class))

(def free 0)
(def robot [1 0]) ;robots default direction is "to the right"
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

(defn get-item 
  "returns item on given position"
  [board [x y]]
  (nth (nth board y) x))

(defn robot?
  "verifies if there's a robot on given position"
  [board [x y]]
  (sequential? (get-item board [x y])))

(defn dino?
  "verifies if there's a dino on given position"
  [board [x y]]
  (= (get-item board [x y]) dino))

(defn free?
  "verifies if given position is free"
  [board [x y]]
  (= (get-item board [x y]) free))

(defn valid?
  "returns true if (x,y) is inside the board"
  [board [x y]]
  (let [dim (count board)]
    (and (>= x 0) (< x dim) (>= y 0) (< y dim))))

(defn prettify-row
  "converts a board row to a string with pretty html symbols"
  [row]
  (str (apply str (map #(if (= % 0) "#" 
                     (if (= % 2) "D" "R")) row)) "<br>"))

(defn pretty-board
  "prettify board for better representation"
  [board]
  (apply str (map prettify-row board)))

;;;;
;; robots operations
;;;;
(def up-vec [0 -1])
(def down-vec [0 1])
(def right-vec [1 0])
(def left-vec [-1 0])

;; movements
(defn move
  "returns new position based on given position and given direction"
  [[x y] dir]
  (map + [x y] dir))

(defn can-move?
  "returns true if robot can move to the given position"
  [board [x y]]
  (and (valid? board [x y]) (free? board [x y])))

(defn do-move
  "returns a new board with robot's position indicated by f.
  returns the board if move is not valid"
  [board [x y] f]
  (if (robot? board [x y])
    (let [item (get-item board [x y])
          dir (map f item)
          pos (move [x y] dir)]
      (if (can-move? board pos)
        (place-in-board (erase-position board [x y]) pos item)
      board))
    board))

(defn fwd-move
  "moves robot forward in according to its current direction"
  [board [x y]]
  (do-move board [x y] identity))
  
(defn rev-move
  "moves robot backwards in according to its current direction"
  [board [x y]]
  (do-move board [x y] -))

;; rotations
(defn turn-right
  [dir]
  (cond
    (= dir up-vec) right-vec
    (= dir right-vec) down-vec
    (= dir down-vec) left-vec
    (= dir left-vec) up-vec))

(defn turn-left
  [dir]
  (into [] (map - (turn-right dir))))

(defn rotate
  "changes robot direction clockwise or counterclockwise
  (determined by f)"
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

;; attack
(defn do-attack
  "erases given position or returns the board if attack is not valid"
  [board [x y]]
  (if (and (valid? board [x y]) (dino? board [x y]))
    (erase-position board [x y])
    board))

(defn attack
  "makes robot from given position attack its adjacent positions"
  [board [x y]]
  (if (robot? board [x y])
    (let [left (move [x y] left-vec)
          right (move [x y] right-vec)
          down (move [x y] down-vec)
          up (move [x y] up-vec)]
      (reduce do-attack board [left right down up]))
    board))

;;;;
;; board state management
;;;;
(def board-dim 50)

(def board-state (atom (new-board board-dim)))

(defn get-state
  "returns the current state of the board"
  [& args]
  @board-state)

(defn change-state
  "receives a board function and change state to the return of this function"
  [f args]
  (swap! board-state f args))

(defn reset-board-state
  "resets board state to default value"
  [& args] ;; TODO: use swap! instead?
  (reset! board-state (new-board board-dim)))

;;;;
;; API logic
;;;;
(defn show-board-state
  []
  (let [response {:status 200
                  :headers {"Content-Type" "text/html"}
                  :body (str "<meta http-equiv=\"refresh\" content=\"2\"><body>" (str (pretty-board (get-state))) "</body>" )}]
    response))

(defn do-action
  [req f]
  (try
    (let [body (json/read-str (slurp (:body req)) :key-fn keyword)
          response {:status 200}]
      (change-state f [(:x body) (:y body)])
      response)
  (catch Exception ex
    ;(log/error "Error on " (:uri req) (.getMessage ex))
    {:status 400
     :body "bad request"})))

(defn place-dino-handler
  [req]
  (do-action req place-dino))

(defn place-robot-handler
  [req]
  (do-action req place-robot))

(defn robot-cmd-handler
  [req]
  (let [cmd (-> req :params :op)]
    (case cmd
      "fwd-move" (do-action req fwd-move)
      "rev-move" (do-action req rev-move)
      "attack" (do-action req attack)
      "rotate-left" (do-action req rotate-left)
      "rotate-right" (do-action req rotate-right)
      {:status 400
       :body "bad request"})))

(defn reset-board-handler
  [& _]
  (let [response {:status 200}]
    (reset-board-state)
    response))

(defroutes app
  (GET "/" [] "<h1>Welcome</h1>")
  (GET "/show-state" [] (show-board-state))
  (POST "/place-dino" [] place-dino-handler)
  (POST "/place-robot" [] place-robot-handler)
  (POST "/robot-cmd/:op" [] robot-cmd-handler)
  (POST "/reset-board" [] reset-board-handler)
  (route/not-found "<h1>Page not found</h1>"))


(defn -main [& args]
    (run-server app {:port 8080})
      (println "Server started on port 8080"))

