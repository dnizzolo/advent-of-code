(defpackage :aoc2022.06
  (:documentation "Tuning Trouble.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.06)

(defun read-datastream-buffer (&optional (relative-pathname #p"2022/inputs/day06.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (uiop:read-file-string filename)))

(defun datastream-start-marker (data &key (distinct-chars-required 4))
  (loop for i from distinct-chars-required below (length data)
        for slice = (subseq data (- i distinct-chars-required) i)
        until (all-different-p slice)
        finally (return i)))

(defun day06 ()
  (let ((data (read-datastream-buffer)))
    (values (datastream-start-marker data)
            (datastream-start-marker data :distinct-chars-required 14))))

(define-test (= 1343) (= 2193))
