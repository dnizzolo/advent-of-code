(defpackage :aoc2023.02
  (:documentation "Cube Conundrum.")
  (:local-nicknames (:omrn :one-more-re-nightmare))
  (:use :cl :aoc.utils))

(in-package :aoc2023.02)

(defun read-games (&optional (relative-pathname #p"2023/inputs/day02.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for subline = (subseq line (1+ (position #\: line)))
            collect (game-to-rgb subline)))))

(defun game-to-rgb (game)
  (flet ((round-to-rgb (round)
           (let ((parts (uiop:split-string round :separator '(#\,)))
                 (regex (omrn:compile-regular-expression "«[0-9]+» «(red|blue|green)»")))
             (loop with r = 0 with g = 0 with b = 0
                   for p in parts
                   do (omrn:do-matches ((i f qi qf ci cf) regex p)
                        (declare (ignorable i f cf))
                        (let ((q (parse-integer p :start qi :end qf)))
                          (ecase (char p ci)
                            (#\r (setf r q))
                            (#\g (setf g q))
                            (#\b (setf b q)))))
                   finally (return (list r g b))))))
    (let ((rounds (uiop:split-string game :separator '(#\;))))
      (loop for round in rounds collect (round-to-rgb round)))))

(defun sum-possible-games (games red green blue)
  (flet ((possible-game-p (game)
           (loop for (r g b) in game never (or (> r red) (> g green) (> b blue)))))
    (loop for id from 1
          for game in games
          when (possible-game-p game) sum id)))

(defun sum-cube-set-power (games)
  (flet ((cube-set-power (game)
           (let ((red (reduce #'max game :key #'first))
                 (green (reduce #'max game :key #'second))
                 (blue (reduce #'max game :key #'third)))
             (* red green blue))))
    (loop for game in games sum (cube-set-power game))))

(defun day02 (&aux (games (read-games)))
  (values (sum-possible-games games 12 13 14) (sum-cube-set-power games)))

(define-test (= 2285) (= 77021))
