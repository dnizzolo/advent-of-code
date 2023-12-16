(defpackage :aoc2023.09
  (:documentation "Mirage Maintenance.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.09)

(defun read-report (&optional (relative-pathname #p"2023/inputs/day09.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (nreverse (parse-integers-from-string line))))))

(defun compute-next-value (history)
  (if (every #'zerop history)
      0
      (+ (first history)
         (compute-next-value (loop for (x y) on history while y collect (- x y))))))

(defun day09 (&aux (report (read-report)))
  (loop for history in report
        sum (compute-next-value history) into part-1
        sum (compute-next-value (reverse history)) into part-2
        finally (return (values part-1 part-2))))

(define-test (= 1992273652) (= 1012))
