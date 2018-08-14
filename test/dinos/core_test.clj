(ns dinos.core-test
  (:require [clojure.test :refer :all]
            [dinos.core :refer :all]))

;; testing board functions
(deftest zeros-test
  (testing "should return lazy sequence of zeros"
    (is (instance? clojure.lang.LazySeq zeros))
    (is (= (take 1 zeros) '(0)))
    (is (= (take 3 zeros) '(0 0 0)))))

(deftest new-board-test
  (testing "creation of new board should return an NxN matrix with only zeros"
    (is (= (new-board 2) '((0 0), (0 0))))
    (is (not= (new-board 2) '((1 1), (1 1))))
    (is (=  (new-board 3) '((0 0 0), (0 0 0), (0 0 0))))))

(deftest place-in-row-test
  (testing "placement of an element on certain position of a list"
    (is (= (place-in-row [1 2] 1 "abc") [1 "abc"]))
    (is (= (place-in-row [1 2 3] 1 0) [1 0 3])))
  (testing "trying to place an element out of range"
    (is (thrown? Exception (place-in-row [1 2] "abc" 4)))))

(deftest place-in-board-test
  (testing "placement of an element of certain position of a grid/board"
    (is (= (place-in-board [[1]] [0 0] "testing") [["testing"]]))
    (is (= (place-in-board [[0 0] [0 0]] [0 0] 1) [[1 0] [0 0]])))
  (testing "trying to place an element out of range"
    (is (thrown? Exception (place-in-board [[1]] [1 2] "testing")))))

(deftest place-dino-test
  (testing "placement of dino into board"
    (is (= (place-dino [[0 0] [0 0]] [0 0]) [[2 0] [0 0]]))))

(deftest place-robot-test
  (testing "placement of robot into board"
    (is (not= (place-robot [[0]] [0 0]) [[[-1 0]]]))
    (is (= (place-robot [[0]] [0 0]) [[[1 0]]]))))

(deftest erase-position-test
 (testing "erasement of a position of the board"
  (is (= (erase-position [[1 0] [0 0]] [0 0]) [[0 0] [0 0]])))) 

(deftest get-item-test
  (testing "if function returns the element of given position of the board"
    (is (= (get-item [[-1]] [0 0]) -1))
    (is (not= (get-item [[1 1] [2 2]] [1 0]) 2))))

(deftest robot?-test
  (testing "if there's a robot on the given position"
    (let [a-board (new-board 10)]
      (is (robot? (place-robot a-board [0 1]) [0 1]))
      (is (not (robot? (place-robot a-board [0 1]) [1 1]))))))

(deftest dino?-test
  (testing "if there's a dino on the given position"
    (let [a-board (new-board 10)]
      (is (dino? (place-dino a-board [0 1]) [0 1]))
      (is (not (dino? (place-robot a-board [0 1]) [0 1]))))))

(deftest free?-test
  (testing "if given position of the board is a zero"
    (let [a-board (new-board 10)]
      (is (free? a-board [0 1]))
      (is (not (free? (place-robot a-board [0 1]) [0 1]))))))

(deftest valid?-test
  (testing "if a position is inside the board"
    (let [a-board (new-board 10)]
      (is (valid? a-board [0 0]))
      (is (valid? a-board [5 5]))
      (is (not (valid? a-board [10 11]))))))

;; testing robots commands

(deftest move-test
  (testing "if function returns the sum of given coordinates with a direction vector"
    (is (= (move [2 2] [-1 0]) [1 2]))
    (is (= (move [0 0] [1 0]) [1 0]))
    (is (= (move [2 2] [0 1]) [2 3] ))))

(deftest can-move?-test
  (let [a-board (new-board 10)]
    (testing "if move is still inside the board"
      (is (can-move? a-board [2 2]))
      (is (can-move? a-board [9 9])))
    (testing "if move is outside the range of the board"
      (is (not (can-move? a-board [10 10])))
      (is (not (can-move? a-board [-1 2]))))))

(deftest do-move-test
  (let [a-board (place-robot (new-board 10) [0 0])
        a-dino-board (place-dino a-board [1 0])]
    (testing "if robot by default moves 'to the right'"
      (is (= (do-move a-board [0 0] identity) (place-robot (new-board 10) [1 0]))))
    (testing "if robot can't visit a place with a dino on it"
      (is (= (do-move a-dino-board [0 0] identity) a-dino-board)))))

(deftest fwd-move-test
  (let [a-board (place-robot (new-board 10) [2 2])]
    (testing "if default robot walks for its 'right' on a fwd-move"
      (is (= (fwd-move a-board [2 2]) (place-robot (new-board 10) [3 2]))))))


(deftest rev-move-test
  (let [a-board (place-robot (new-board 10) [2 2])]
    (testing "if default robot walks for its 'left' on a rev-move"
      (is (= (rev-move a-board [2 2]) (place-robot (new-board 10) [1 2]))))))

(deftest turn-right-test
  (testing "if direction vector turns to the right"
    (is (= (turn-right [1 0]) [0 1]))
    (is (= (turn-right [0 1]) [-1 0]))
    (is (= (turn-right [-1 0]) [0 -1]))
    (is (= (turn-right [0 -1]) [1 0]))))

(deftest turn-left-test
  (testing "if direction vector turns to the left"
    (is (= (turn-left [1 0]) [0 -1]))
    (is (= (turn-left [0 -1]) [-1 0]))
    (is (= (turn-left [-1 0]) [0 1]))
    (is (= (turn-left [0 1]) [1 0]))))

(deftest rotate-right-test
  (testing "if robots change it's direction vector for a right rotation"
    (let [a-board (place-robot (new-board 10) [2 2])]
      (is (= (get-item (rotate-right a-board [2 2]) [2 2]) [0 1])))))


(deftest rotate-left-test
  (testing "if robots change it's direction vector for a left rotation"
    (let [a-board (place-robot (new-board 10) [2 2])]
      (is (= (get-item (rotate-left a-board [2 2]) [2 2]) [0 -1])))))

(deftest do-attack-test
  (testing "if dinos are erased from given position"
    (let [a-board (place-dino (new-board 10) [1 2])]
      (is (= (new-board 10) (do-attack a-board [1 2]))))))


(deftest attack-test
  (testing "if robot erases all dinos from (only) its adjacent cells"
    (let [a-board (place-robot (new-board 10) [2 2])
          a-dino-board (place-dino a-board [1 2])
          an-away-dino-board (place-dino a-board [5 5])]
      (is (= (attack an-away-dino-board [2 2]) an-away-dino-board))
      (is (= (attack a-dino-board [2 2]) a-board)))))



