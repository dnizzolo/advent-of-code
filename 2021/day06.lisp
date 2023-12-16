(defpackage :aoc2021.06
  (:documentation "Lanternfish.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.06)

(defun read-lanternfish-initial-population (&optional (rel-path #p"2021/inputs/day06.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code rel-path)))
    (with-open-file (in filename)
      (parse-integers-from-string (read-line in nil nil)))))

(defun make-fish-counter (fish-list)
  (loop with array = (make-array 9)
        for fish in fish-list
        do (incf (aref array fish))
        finally (return array)))

(defun evolve-fishes (fish-list days)
  (loop with fishes = (make-fish-counter fish-list)
        with zero-index = 0
        repeat days
        do (incf (aref fishes (mod (+ zero-index 7) 9))
                 (aref fishes zero-index))
           (setf zero-index (mod (1+ zero-index) 9))
        finally (return (reduce #'+ fishes))))

(defun day06 ()
  (let ((fish-list (read-lanternfish-initial-population)))
    (values
     (evolve-fishes fish-list 80)
     (evolve-fishes fish-list 256))))

(define-test (= 377263) (= 1695929023803))
