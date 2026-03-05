(defpackage #:aoc2025.01
  (:documentation "Secret Entrance.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.01)

(defun read-instructions (&optional (relative-pathname #p"2025/inputs/day01.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for amount = (parse-integer line :start 1)
            collect (if (char= (char line 0) #\L) (- amount) amount)))))

(defun apply-instruction (state instruction)
  (mod (+ state instruction) 100))

(defun password (instructions)
  (loop for state = 50 then (apply-instruction state inst)
        for inst in instructions
        count (zerop state)))

(defun other-password (instructions)
  (loop with state = 50
        for inst in instructions
        for next-state = (apply-instruction state inst)
        sum (+ (if (zerop state) 1 0)
               (floor (abs inst) 100)
               (cond ((and (minusp inst) (< 0 state next-state)) 1)
                     ((and (plusp inst) (< 0 next-state state)) 1)
                     (t 0)))
        do (setf state next-state)))

(defun day01 (&aux (instructions (read-instructions)))
  (values (password instructions) (other-password instructions)))

(define-test (= 989) (= 5941))
