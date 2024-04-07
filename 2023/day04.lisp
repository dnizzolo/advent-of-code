(defpackage :aoc2023.04
  (:documentation "Scratchcards.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.04)

(defun read-scratchcards (&optional (relative-pathname #p"2023/inputs/day04.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (length
                     (intersection
                      (parse-integers line
                                      :start (position #\: line)
                                      :end (position #\| line))
                      (parse-integers line
                                      :start (position #\| line))))))))

(defun scratchcards-points (scratchcards)
  (loop for wins in scratchcards sum (ash 1 (1- wins))))

(defun total-scratchcards (scratchcards)
  (let* ((len (length scratchcards))
         (counter (make-array len :initial-element 1)))
    (loop for i from 0
          for wins in scratchcards
          for existing = (aref counter i)
          do (loop for j from (1+ i) to (min (+ i wins) len)
                   do (incf (aref counter j) existing))
          finally (return (reduce #'+ counter)))))

(defun day04 (&aux (scratchcards (read-scratchcards)))
  (values (scratchcards-points scratchcards) (total-scratchcards scratchcards)))

(define-test (= 25004) (= 14427616))
