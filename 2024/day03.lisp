(defpackage :aoc2024.03
  (:documentation "Mull It Over.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.03)

(defun read-memory (&optional (relative-pathname #p"2024/inputs/day03.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (apply #'concatenate 'string (uiop:read-file-lines filename))))

(defun sum-of-multiplications (memory &aux (result 0))
  (ppcre:do-register-groups ((#'parse-integer x y))
      ((ppcre:create-scanner "mul\\((\\d+),(\\d+)\\)") memory)
    (incf result (* x y)))
  result)

(defun day03 (&aux (memory (read-memory)))
  (values (sum-of-multiplications memory)
          (sum-of-multiplications
           (ppcre:regex-replace-all
            (ppcre:create-scanner "don't\\(\\).*?(?:do\\(\\)|\\Z)") memory ""))))

(define-test (= 173517243) (= 100450138))
