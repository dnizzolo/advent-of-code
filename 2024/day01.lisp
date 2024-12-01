(defpackage :aoc2024.01
  (:documentation "Historian Histeria.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.01)

(defun read-lists (&optional (relative-pathname #p"2024/inputs/day01.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for (x y) = (parse-integers line)
            collect x into left
            collect y into right
            finally (return (values left right))))))

(defun day01 ()
  (multiple-value-bind (left right) (read-lists)
    (setf left (sort left #'<)
          right (sort right #'<))
    (values (reduce #'+ (mapcar (lambda (x y) (abs (- x y))) left right))
            (reduce #'+ (mapcar (lambda (x) (* x (count x right))) left)))))

(define-test (= 936063) (= 23150395))
