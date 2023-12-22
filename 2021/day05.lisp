(defpackage :aoc2021.05
  (:documentation "Hydrothermal Venture.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.05)

(defun read-hydrovents (&optional (rel-path #p"2021/inputs/day05.txt")
                        &aux (filename (asdf:system-relative-pathname :advent-of-code rel-path)))
  (mapcar (lambda (lis)
            (destructuring-bind (x1 y1 x2 y2) lis
              (list (list x1 y1) (list x2 y2))))
          (with-open-file (in filename)
            (loop for line = (read-line in nil)
                  while line
                  collect (parse-integers-from-string line)))))

(defun straightp (pair)
  (or (= (caar pair) (caadr pair)) (= (cadar pair) (cadadr pair))))

(defun make-straight-hydrovents-hash-table (hydrovents)
  (let ((tbl (make-hash-table :test #'equal)))
    (loop for pair in hydrovents
          for (x1 y1) = (car pair)
          for (x2 y2) = (cadr pair)
          when (straightp pair)
            do (loop for x from (min x1 x2) to (max x1 x2)
                     do (loop for y from (min y1 y2) to (max y1 y2)
                              do (incf (gethash (list x y) tbl 0)))))
    tbl))

(defun left-x-point (pair)
  (if (< (caar pair) (caadr pair)) (car pair) (cadr pair)))

(defun right-x-point (pair)
  (if (> (caar pair) (caadr pair)) (car pair) (cadr pair)))

(defun make-hydrovents-hash-table (hydrovents)
  (let ((tbl (make-straight-hydrovents-hash-table hydrovents))
        (remaining (remove-if #'straightp hydrovents)))
    (loop for pair in remaining
          for (x1 y1) = (left-x-point pair)
          for (x2 y2) = (right-x-point pair)
          if (< y1 y2)
            do (loop for x from x1 to x2
                     for y from y1 to y2
                     do (incf (gethash (list x y) tbl 0)))
          else
            do (loop for x from x1 to x2
                     for y from y1 downto y2
                     do (incf (gethash (list x y) tbl 0))))
    tbl))

(defun count-overlapping-points (tbl)
  (loop for v being the hash-values of tbl count (> v 1)))

(defun day05 ()
  (let ((vents (read-hydrovents)))
    (values
     (count-overlapping-points (make-straight-hydrovents-hash-table vents))
     (count-overlapping-points (make-hydrovents-hash-table vents)))))

(define-test (= 5373) (= 21514))
