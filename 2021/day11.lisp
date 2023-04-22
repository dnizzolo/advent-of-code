(defpackage :aoc2021.11
  (:documentation "Dumbo Octopus.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.11)

(defun read-dumbo-octopuses (&optional (relative-path #p"2021/inputs/day11.txt"))
  (let* ((filename (asdf:system-relative-pathname :advent-of-code relative-path))
         (octopuses (with-open-file (in filename)
                      (loop for line = (read-line in nil nil)
                            while line
                            collect (map 'list #'digit-char-p line)))))
    (make-array (list (length octopuses) (length (first octopuses)))
                :initial-contents octopuses)))

(defun octopus-neighbours (octopuses i j)
  (declare (type (integer 0) i j))
  (destructuring-bind (rows cols) (array-dimensions octopuses)
    (loop for (di dj) in '((1 0) (1 1) (-1 0) (0 -1) (0 1) (-1 1) (-1 -1) (1 -1))
          for ni = (+ i di) and nj = (+ j dj)
          when (and (< -1 ni rows) (< -1 nj cols))
            collect (list ni nj))))

(defun flash-octopuses (octopuses i j)
  (when (> (aref octopuses i j) 9)
    (setf (aref octopuses i j) 0)
    (loop for (ni nj) in (octopus-neighbours octopuses i j)
          when (plusp (aref octopuses ni nj))
            do (incf (aref octopuses ni nj))
               (flash-octopuses octopuses ni nj))))

(defun evolve-octopuses (octopuses)
  (destructuring-bind (rows cols) (array-dimensions octopuses)
    (loop for i below rows do (loop for j below cols do (incf (aref octopuses i j))))
    (loop for i below rows do (loop for j below cols do (flash-octopuses octopuses i j)))))

(defun count-flashed-octopuses (octopuses)
  (destructuring-bind (rows cols) (array-dimensions octopuses)
    (loop for i below rows sum (loop for j below cols count (zerop (aref octopuses i j))))))

(defun day11/part-1 (octopuses)
  (loop repeat 100
        do (evolve-octopuses octopuses)
        sum (count-flashed-octopuses octopuses)))

(defun every-octopus-flashed (octopuses)
  (destructuring-bind (rows cols) (array-dimensions octopuses)
    (loop for i below rows
          always (loop for j below cols
                       always (zerop (aref octopuses i j))))))

(defun day11/part-2 (octopuses)
  (loop for step from 0
        until (every-octopus-flashed octopuses)
        do (evolve-octopuses octopuses)
        finally (return step)))

(defun day11 ()
  (values (day11/part-1 (read-dumbo-octopuses)) (day11/part-2 (read-dumbo-octopuses))))

(define-test (= 1747) (= 505))
