(defpackage :aoc2024.22
  (:documentation "Monkey Market.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2024.22)

(defun read-initial-numbers (&optional (relative-pathname #p"2024/inputs/day22.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (mapcar #'parse-integer (uiop:read-file-lines filename))))

(defun prune (number)
  (mod number 16777216))

(defun mix (value number)
  (logxor value number))

(defun tick (number)
  (setf number (prune (mix number (* number 64))))
  (setf number (prune (mix number (floor number 32))))
  (setf number (prune (mix number (* number 2048)))))

(defun secret-numbers (number)
  (loop with result = (make-array 2001 :element-type 'integer)
        initially (setf (aref result 0) number)
        for i from 1 to 2000
        do (setf (aref result i) (tick (aref result (1- i))))
        finally (return result)))

(defun changes (numbers)
  (loop with length = (1- (length numbers))
        with result = (make-array length :element-type 'integer)
        for i from 1 below length
        do (setf (aref result (1- i)) (- (aref numbers i) (aref numbers (1- i))))
        finally (return result)))

(defun score (sequence changes)
  (loop with length = (- (length changes) 3)
        with result = (make-hash-table :test #'equal)
        for i below length
        for pattern = (list (aref changes i) (aref changes (1+ i))
                            (aref changes (+ i 2)) (aref changes (+ i 3)))
        unless (gethash pattern result)
          do (setf (gethash pattern result) (aref sequence (+ i 4)))
        finally (return result)))

(defun day22 (&aux (numbers (read-initial-numbers)))
  (loop with score = (make-hash-table :test #'equal)
        for number in numbers
        for secret-sequence = (secret-numbers number)
        for real-sequence = (map 'vector (lambda (n) (mod n 10)) secret-sequence)
        for changes = (changes real-sequence)
        for scores = (score real-sequence changes)
        do (maphash (lambda (key value) (incf (gethash key score 0) value)) scores)
        sum (aref secret-sequence 2000) into part-1
        finally (return (values part-1 (reduce #'max (a:hash-table-values score))))))

(define-test (= 15303617151) (= 1727))
