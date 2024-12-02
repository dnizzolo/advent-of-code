(defpackage :aoc2024.02
  (:documentation "Red-Nosed Reports.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.02)

(defun read-reports (&optional (relative-pathname #p"2024/inputs/day02.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (parse-integers line)))))

(defun safe-report-p (report)
  (and (or (apply #'< report)
           (apply #'> report))
       (loop for (x y) on report while y always (<= 1 (abs (- x y)) 3))))

(defun dampened-safe-report-p (report)
  (or (safe-report-p report)
      (loop for level in report
            for i below (length report)
            thereis (safe-report-p (remove level report :start i :count 1)))))

(defun day02 (&aux (reports (read-reports)))
  (values (count-if #'safe-report-p reports)
          (count-if #'dampened-safe-report-p reports)))

(define-test (= 432) (= 488))
