(defpackage :aoc2021.19
  (:documentation "Beacon Scanner.")
  (:use :cl :aoc.utils)
  (:local-nicknames (:m :magicl)))

(in-package :aoc2021.19)

(defstruct scanner
  id
  points)

(defun read-scanners (&optional (relative-path #p"2021/inputs/day19.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop for id from 0
            for points = (loop initially (read-line in nil)
                               for line = (read-line in nil)
                               while (plusp (length line))
                               collect (m:from-list
                                        (parse-integers line)
                                        '(3)
                                        :type 'double-float))
            while points
            collect (make-scanner :id id :points points)))))

(defun compute-3-cube-rotations ()
  (flet ((matrix3-equal (m1 m2)
           (and (= (m:tref m1 0 0) (m:tref m2 0 0)) (= (m:tref m1 0 1) (m:tref m2 0 1))
                (= (m:tref m1 0 2) (m:tref m2 0 2)) (= (m:tref m1 1 0) (m:tref m2 1 0))
                (= (m:tref m1 1 1) (m:tref m2 1 1)) (= (m:tref m1 1 2) (m:tref m2 1 2))
                (= (m:tref m1 2 0) (m:tref m2 2 0)) (= (m:tref m1 2 1) (m:tref m2 2 1))
                (= (m:tref m1 2 2) (m:tref m2 2 2)))))
    (let ((eye (m:eye '(3 3)))
          (x90 (m:from-list '(1 0 0 0 0 -1 0 1 0) '(3 3) :type 'double-float))
          (y90 (m:from-list '(0 0 1 0 1 0 -1 0 0) '(3 3) :type 'double-float))
          (z90 (m:from-list '(0 -1 0 1 0 0 0 0 1) '(3 3) :type 'double-float)))
      (loop with result
            repeat 4 for e1 = eye then (m:@ e1 x90)
            do (loop repeat 4 for e2 = e1 then (m:@ e2 y90)
                     do (loop repeat 4 for e3 = e2 then (m:@ e3 z90)
                              do (push e3 result)))
            finally (return (remove-duplicates result :test #'matrix3-equal))))))

(defvar *3-cube-rotations* (compute-3-cube-rotations))
