(defpackage #:aoc2022.25
  (:documentation "Full of Hot Air.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2022.25)

(defun read-fuel-required (&optional (relative-pathname #p"2022/inputs/day25.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (uiop:read-file-lines filename)))

(defun snafu->decimal (snafu)
  (loop for i from (1- (length snafu)) downto 0
        for char = (char snafu i)
        for fact = 1 then (* 5 fact)
        sum (* fact (ecase char
                      ((#\0 #\1 #\2) (digit-char-p char))
                      (#\= -2) (#\- -1)))))

(defun decimal->snafu (decimal)
  (loop with snafu = (make-array 0 :element-type 'character
                                   :adjustable t :fill-pointer t)
        with inc
        for (q r) = (multiple-value-list (floor decimal 5))
        do (setf decimal q)
           (when inc (incf r))
           (setf inc (or (= r 3) (= r 4) (= r 5))
                 r (mod r 5))
           (vector-push-extend (ecase r
                                 ((0 1 2) (digit-char r))
                                 (3 #\=) (4 #\-))
                               snafu)
        until (zerop q)
        finally (when inc (vector-push-extend #\1 snafu))
                (return (nreverse snafu))))

(defun day25 ()
  (let* ((required (read-fuel-required))
         (sum (reduce #'+ required :key #'snafu->decimal)))
    (decimal->snafu sum)))

(define-test (string= "2-0-0=1-0=2====20=-2"))
