(defpackage :aoc2022.13
  (:documentation "Distress Signal.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.13)

(defun read-packets (&optional (relative-pathname #p"2022/inputs/day13.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (read-from-string
     (format nil "(~a)"
             (map 'string
                  (lambda (x)
                    (case x
                      (#\, #\Space)
                      (#\[ #\()
                      (#\] #\))
                      (t x)))
                  (uiop:read-file-string filename))))))

(defgeneric compare-packet (left right)
  (:method ((left integer) (right integer))
    (cond ((< left right) :good)
          ((> left right) :bad)))
  (:method ((left integer) (right list))
    (compare-packet (list left) right))
  (:method ((left list) (right integer))
    (compare-packet left (list right)))
  (:method ((left list) (right list))
    (cond ((and (null left) right) :good)
          ((and left (null right)) :bad)
          ((and left right)
           (or (compare-packet (car left) (car right))
               (compare-packet (cdr left) (cdr right)))))))

(defun solve-part-1 (packets)
  (loop for i from 1
        for (left right) on packets by #'cddr
        when (eq (compare-packet left right) :good)
          sum i))

(defun solve-part-2 (packets)
  (let* ((start '((2))) (end '((6)))
         (packets (sort (nconc (list start end) packets)
                        (lambda (x y) (eq (compare-packet x y) :good)))))
    (* (1+ (position start packets :test #'tree-equal))
       (1+ (position end packets :test #'tree-equal)))))

(defun day13 (&aux (packets (read-packets)))
  (values (solve-part-1 packets) (solve-part-2 packets)))

(define-test (= 5506) (= 21756))
