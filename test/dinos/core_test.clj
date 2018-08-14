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
  (testing "TODO"
    (is (= 1 1))))























