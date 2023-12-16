(defpackage :aoc2021.01
  (:documentation "Sonar Sweep.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.01)

(defun read-sonar-depths ()
  (mapcar #'parse-integer
          (uiop:read-file-lines
           (asdf:system-relative-pathname
            :advent-of-code "2021/inputs/day01.txt"))))

(defun day01 ()
  (let ((depths (read-sonar-depths)))
    (values
     (loop for (a b) on depths while b count (< a b))
     (loop for (a b c d) on depths while d count (< a d)))))

(define-test (= 1195) (= 1235))
