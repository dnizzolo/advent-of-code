(defpackage :aoc2021.07
  (:documentation "The Treachery of Whales.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.07)

;;;; Solved the optimization problems by hand.

(defun read-crab-positions (&optional (rel-path #p"2021/inputs/day07.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code rel-path)))
    (with-open-file (in filename)
      (parse-integers-from-string (read-line in)))))

(defun day07/part-1 (crabs)
  (loop with median = (a:median crabs)
        for position in (list (floor median) (ceiling median))
        minimize (loop for crab in crabs sum (abs (- crab position)))))

(defun day07/part-2 (crabs)
  (loop with mean = (a:mean crabs)
        for position in (list (floor mean) (ceiling mean))
        minimize (loop for crab in crabs
                       for dist = (abs (- crab position))
                       sum (triangular dist))))

(defun day07 (&aux (crabs (read-crab-positions)))
  (values (day07/part-1 crabs) (day07/part-2 crabs)))

(define-test (= 359648) (= 100727924))
