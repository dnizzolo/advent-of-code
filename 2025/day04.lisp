(defpackage #:aoc2025.04
  (:documentation "Printing Department.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.04)

(defun read-diagram (&optional (relative-pathname #p"2025/inputs/day04.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with grid = (make-hash-table :test #'equal)
            for line = (read-line in nil)
            for i from 0
            while line
            do (loop for j from 0
                     for c across line
                     when (char= c #\@)
                       do (setf (gethash (cons i j) grid) t))
            finally (return grid)))))

(defun tick (grid)
  (loop with new-grid = (make-hash-table :test #'equal)
        with removed = 0
        for (i . j) being the hash-keys of grid
        if (>= (loop for (ni nj) in (list (list (1+ i) j) (list (1- i) j)
                                          (list i (1+ j)) (list i (1- j))
                                          (list (1+ i) (1+ j)) (list (1+ i) (1- j))
                                          (list (1- i) (1+ j)) (list (1- i) (1- j)))
                     count (gethash (cons ni nj) grid))
               4)
          do (setf (gethash (cons i j) new-grid) t)
        else
          do (incf removed)
        finally (return (values new-grid removed))))

(defun find-stable (grid)
  (loop for (new-grid removed) = (multiple-value-list (tick grid))
        until (zerop removed)
        sum removed
        do (setf grid new-grid)))

(defun day04 (&aux (diagram (read-diagram)))
  (values (nth-value 1 (tick diagram)) (find-stable diagram)))

(define-test (= 1320) (= 8354))
