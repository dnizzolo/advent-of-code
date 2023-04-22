(defpackage :aoc2022.03
  (:documentation "Rucksack Reorganization.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.03)

(defun read-rucksack-contents (&optional (relative-pathname #p"2022/inputs/day03.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line collect line))))

(defun rucksack-item-priority (item)
  (- (char-code item)
     (if (upper-case-p item)
         #.(- (char-code #\A) 27)
         #.(- (char-code #\a) 1))))

(defun sum-of-common-item-priority (items)
  (loop for line in items
        for half = (/ (length line) 2)
        sum (loop for c across line
                  until (find c line :start half)
                  finally (return (rucksack-item-priority c)))))

(defun sum-of-triplets-priority (items)
  (loop for (e1 e2 e3) on items by #'cdddr
        while e3
        sum (loop for c across e1
                  until (and (find c e2) (find c e3))
                  finally (return (rucksack-item-priority c)))))

(defun day03 ()
  (let ((items (read-rucksack-contents)))
    (values (sum-of-common-item-priority items)
            (sum-of-triplets-priority items))))

(define-test (= 7737) (= 2697))
