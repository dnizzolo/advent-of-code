(defpackage :aoc2023.01
  (:documentation "Trebuchet?!")
  (:local-nicknames (:s :serapeum))
  (:use :cl :aoc.utils))

(in-package :aoc2023.01)

(defun read-calibration-document (&optional (relative-pathname #p"2023/inputs/day01.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (uiop:read-file-lines filename)))

(defun old-calibration (line)
  (let* ((string (remove-if-not #'digit-char-p line))
         (last-index (1- (length string))))
    (+ (* 10 (digit-char-p (char string 0)))
       (digit-char-p (char string last-index)))))

(defvar *replacements*
  '(("one" "o1e") ("two" "t2o") ("three" "t3e")
    ("four" "4") ("five" "5e") ("six" "6")
    ("seven" "7n") ("eight" "e8t") ("nine" "n9e")))

(defun new-calibration (line)
  (loop for (text replace) in *replacements*
        do (setf line (s:string-replace-all text line replace))
        finally (return (old-calibration line))))

(defun sum-of-calibration-values (document calibration-function)
  (loop for line in document
        sum (funcall calibration-function line)))

(defun day01 (&aux (document (read-calibration-document)))
  (values (sum-of-calibration-values document #'old-calibration)
          (sum-of-calibration-values document #'new-calibration)))

(define-test (= 54338) (= 53389))
