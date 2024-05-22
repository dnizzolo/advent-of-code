(defpackage :aoc2023.02
  (:documentation "Cube Conundrum.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.02)

(defun read-games (&optional (relative-pathname #p"2023/inputs/day02.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for subline = (subseq line (1+ (position #\: line)))
            collect (game->rgb subline)))))

(defun game->rgb (game)
  (flet ((round->rgb (round)
           (let ((parts (uiop:split-string round :separator '(#\,)))
                 (regex (ppcre:create-scanner "(\\d+) (red|blue|green)")))
             (loop with r = 0 with g = 0 with b = 0
                   for part in parts
                   do (ppcre:register-groups-bind ((#'parse-integer qty) color)
                          (regex part)
                        (unless color (error "Expected valid color match."))
                        (ecase (char color 0)
                          (#\r (setf r qty))
                          (#\g (setf g qty))
                          (#\b (setf b qty))))
                   finally (return (list r g b))))))
    (let ((rounds (uiop:split-string game :separator '(#\;))))
      (loop for round in rounds collect (round->rgb round)))))

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
