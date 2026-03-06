(defpackage #:aoc2025.03
  (:documentation "Lobby.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.03)

(defun read-banks (&optional (relative-pathname #p"2025/inputs/day03.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (map 'vector #'digit-char-p line)))))

(defun largest-joltage (bank number-of-digits)
  (loop with length = (length bank)
        for i from (1- number-of-digits) downto 0
        for end = (- length i)
        for start = 0 then (1+ (position max bank :start start :end end))
        for max = (reduce #'max bank :start start :end end)
        for result = max then (+ (* result 10) max)
        finally (return result)))

(defun day03 (&aux (banks (read-banks)))
  (loop for bank in banks
        sum (largest-joltage bank 2) into part-1
        sum (largest-joltage bank 12) into part-2
        finally (return (values part-1 part-2))))

(define-test (= 17316) (= 171741365473332))
