(defpackage :aoc2022.04
  (:documentation "Camp Cleanup.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.04)

(defun read-cleanup-assignment-pairs (&optional (relative-pathname #p"2022/inputs/day04.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            do (dotimes (i (length line))
                 (when (char= (char line i) #\-)
                   (setf (char line i) #\Space)))
            collect (parse-integers-from-string line)))))

(defun day04 ()
  (let ((pairs (read-cleanup-assignment-pairs)))
    (values
     (loop for (a b c d) in pairs count (or (<= a c d b) (<= c a b d)))
     (loop for (a b c d) in pairs count (or (<= a c b) (<= c a d))))))

(define-test (= 562) (= 924))
