(defpackage :aoc2022.02
  (:documentation "Rock Paper Scissors.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.02)

(defun read-rock-paper-scissors-strategy (&optional (relative-pathname #p"2022/inputs/day02.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for opponent = (- (char-code (char line 0)) #.(1- (char-code #\A)))
            for mine = (- (char-code (char line 2)) #.(1- (char-code #\X)))
            collect (list opponent mine)))))

(defun rock-paper-scissors-outcome (p1 p2 &aux (dist (- p1 p2)))
  (ecase dist
    ((2 -1) (+ 6 p2))
    ((-2 1) p2)
    (0 (+ 3 p2))))

(defun total-games-scores (strat)
  (loop for (o m) in strat sum (rock-paper-scissors-outcome o m)))

(defun convert-strategy (move outcome)
  (ecase outcome
    (2 (list move move))
    (1 (list move (if (= move 1) 3 (1- move))))
    (3 (list move (1+ (mod move 3))))))

(defun day02 ()
  (let ((strat (read-rock-paper-scissors-strategy)))
    (values (total-games-scores strat)
            (total-games-scores (loop for (i r) in strat collect (convert-strategy i r))))))

(define-test (= 15523) (= 15702))
