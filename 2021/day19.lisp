(defpackage :aoc2021.19
  (:documentation "Beacon Scanner.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.19)

(defun read-scanners (&optional (relative-path #p"2021/inputs/day19.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop for id from 0
            for scanner = (loop initially (read-line in nil)
                                for line = (read-line in nil)
                                while (plusp (length line))
                                collect (parse-integers-from-string line))
            while scanner
            collect (cons id scanner)))))

(defun id (scanner) (car scanner))
(defun beacons (scanner) (cdr scanner))

(defun v- (v w) (mapcar #'- v w))
(defun transpose (m) (apply #'mapcar #'list m))
(defun dot-product (v w) (reduce #'+ (mapcar #'* v w)))
(defun m* (a b &aux (bt (transpose b)))
  (loop for row in a
        collect (loop for col in bt
                      collect (dot-product row col))))
(defun compute-all-3-cube-rotations (&aux (ide '((1 0 0) (0 1 0) (0 0 1)))
                                          (x90 '((1 0 0) (0 0 -1) (0 1 0)))
                                          (y90 '((0 0 1) (0 1 0) (-1 0 0)))
                                          (z90 '((0 -1 0) (1 0 0) (0 0 1))))
  (loop with result = '()
        repeat 4 for e1 = ide then (m* e1 x90)
        do (loop repeat 4 for e2 = e1 then (m* e2 y90)
                 do (loop repeat 4 for e3 = e2 then (m* e3 z90)
                          do (push e3 result)))
        finally (return (remove-duplicates result :test #'equal))))

(defvar *3-cube-rotations* (compute-all-3-cube-rotations))

