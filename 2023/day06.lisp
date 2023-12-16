(defpackage :aoc2023.06
  (:documentation "Wait For It.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.06)

(defun read-time-table (&optional single-race-p (relative-pathname #p"2023/inputs/day06.txt"))
  (flet ((read-line-for-table (stream)
           (let ((s (read-line stream)))
             (if single-race-p (delete #\Space s) s))))
    (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
      (with-open-file (in filename)
        (let ((time (parse-integers-from-string (read-line-for-table in)))
              (distance (parse-integers-from-string (read-line-for-table in))))
          (if single-race-p
              (cons (first time) (first distance))
              (mapcar #'cons time distance)))))))

(defun ways-to-win (time distance)
  (let ((delta (- (* time time) (* 4 distance))))
    (if (plusp delta)
        (let ((t1 (/ (+ time (sqrt delta)) 2))
              (t2 (/ (- time (sqrt delta)) 2)))
          (+ 1 (floor t1) (- (floor (1+ t2)))))
        0)))

(defun day06 ()
  (values (reduce #'* (read-time-table) :key (lambda (c) (ways-to-win (car c) (cdr c))))
          (destructuring-bind (time . distance) (read-time-table t)
            (ways-to-win time distance))))

(define-test (= 3317888) (= 24655068))
