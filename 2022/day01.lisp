(defpackage :aoc2022.01
  (:documentation "Calorie Counting.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.01)

(defun read-calories (&optional (relative-pathname #p"2022/inputs/day01.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop while (listen in)
            collect (loop for line = (read-line in nil)
                          while (plusp (length line))
                          sum (parse-integer line))
              into els
            finally (return (make-array (length els) :initial-contents els))))))

(defun day01 ()
  (let ((cals (subseq (sort (read-calories) #'>) 0 3)))
    (values (svref cals 0) (reduce #'+ cals))))

(define-test (= 67016) (= 200116))
