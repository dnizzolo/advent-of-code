(defpackage :aoc2024.11
  (:documentation "Plutonian Pebbles.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.11)

(defun read-stones (&optional (relative-pathname #p"2024/inputs/day11.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (make-counter (parse-integers (uiop:read-file-line filename)))))

(defun blink (stones &aux (blinked (make-hash-table)))
  (maphash
   (lambda (number amount)
     (if (zerop number)
         (incf (gethash 1 blinked 0) amount)
         (let ((digits (1+ (floor (log number 10)))))
           (if (evenp digits)
               (let ((power (expt 10 (ash digits -1))))
                 (incf (gethash (floor number power) blinked 0) amount)
                 (incf (gethash (mod number power) blinked 0) amount))
               (incf (gethash (* number 2024) blinked 0) amount)))))
   stones)
  blinked)

(defun blinks (stones blinks)
  (loop for current = stones then (blink current)
        repeat blinks
        finally (return (loop for amount being the hash-values of current sum amount))))

(defun day11 (&aux (stones (read-stones)))
  (values (blinks stones 25) (blinks stones 75)))

(define-test (= 218956) (= 259593838049805))
