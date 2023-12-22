(defpackage :aoc2021.10
  (:documentation "Syntax Scoring.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.10)

(defun read-navigation-lines (&optional (relative-path #p"2021/inputs/day10.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect line))))

(defvar *matching-character*
  #.(let ((tbl (make-hash-table)))
      (loop for (key . value) in '((#\( . #\)) (#\[ . #\]) (#\{ . #\}) (#\< . #\>))
            do (setf (gethash key tbl) value))
      tbl))

(defvar *illegal-character-score*
  #.(let ((tbl (make-hash-table)))
      (loop for (key . value) in '((#\) . 3) (#\] . 57) (#\} . 1197) (#\> . 25137))
            do (setf (gethash key tbl) value))
      tbl))

(defvar *completion-score*
  #.(let ((tbl (make-hash-table)))
      (loop for (key . value) in '((#\) . 1) (#\] . 2) (#\} . 3) (#\> . 4))
            do (setf (gethash key tbl) value))
      tbl))

(defun corrupted-line-p (line &aux stack)
  (loop for char across line
        if (gethash char *matching-character*)
          do (push char stack)
        else
          unless (char= (gethash (pop stack) *matching-character*) char)
            return (gethash char *illegal-character-score*)))

(defun incomplete-line-p (line &aux stack)
  (loop for char across line
        if (member char '(#\( #\[ #\{ #\<))
          do (push char stack)
        else
          do (pop stack))
  (mapcar (lambda (char) (gethash char *matching-character*)) stack))

(defun completion-score (completion)
  (reduce (lambda (acc new) (+ (* acc 5) (gethash new *completion-score*)))
          completion
          :initial-value 0))

(defun day10/part-1 (lines)
  (loop for line in lines when (corrupted-line-p line) sum it))

(defun day10/part-2 (lines)
  (let* ((uncorrupted (remove-if #'corrupted-line-p lines))
         (scores (loop for line in uncorrupted
                       for completion = (incomplete-line-p line)
                       when completion
                         collect (completion-score completion)))
         (sorted (sort scores #'<)))
    (nth (ash (length sorted) -1) sorted)))

(defun day10 ()
  (let ((lines (read-navigation-lines)))
    (values (day10/part-1 lines) (day10/part-2 lines))))

(define-test (= 316851) (= 2182912364))
