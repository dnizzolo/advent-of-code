(defpackage :aoc2024.07
  (:documentation "Bridge Repair.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.07)

(defun read-equations (&optional (relative-pathname #p"2024/inputs/day07.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (parse-integers line)))))

(defun valid-equation-p (target operands &optional include-concatenation)
  (if (null operands)
      (zerop target)
      (let ((operand (first operands)))
        (or (and (>= target operand)
                 (valid-equation-p
                  (- target operand)
                  (rest operands)
                  include-concatenation))
            (and (zerop (mod target operand))
                 (valid-equation-p
                  (/ target operand)
                  (rest operands)
                  include-concatenation))
            (and include-concatenation
                 (let* ((digits (1+ (floor (log operand 10))))
                        (power (expt 10 digits))
                        (lower (mod target power)))
                   (and (= lower operand)
                        (valid-equation-p
                         (floor target power)
                         (rest operands)
                         include-concatenation))))))))

(defun day07 (&aux (equations (read-equations)))
  (loop for equation in equations
        for target = (first equation)
        for operands = (reverse (rest equation))
        when (valid-equation-p target operands)
          sum target into part-1
        when (valid-equation-p target operands t)
          sum target into part-2
        finally (return (values part-1 part-2))))

(define-test (= 4364915411363) (= 38322057216320))
