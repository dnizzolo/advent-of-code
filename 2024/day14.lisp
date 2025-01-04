(defpackage :aoc2024.14
  (:documentation "Restroom Redoubt.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.14)

(defun read-robots (&optional (relative-pathname #p"2024/inputs/day14.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (mapcar #'parse-integers (uiop:read-file-lines filename))))

(defconstant +height+ 103)
(defconstant +width+ 101)
(defconstant +mid-height+ 51)
(defconstant +mid-width+ 50)

(defun safety-factor (positions)
  (let ((q1 0) (q2 0) (q3 0) (q4 0))
    (maphash (lambda (pos count)
               (destructuring-bind (px py) pos
                 (cond ((and (< -1 px +mid-width+) (< -1 py +mid-height+))
                        (incf q1 count))
                       ((and (< -1 px +mid-width+) (< +mid-height+ py +height+))
                        (incf q2 count))
                       ((and (< +mid-width+ px +width+) (< -1 py +mid-height+))
                        (incf q3 count))
                       ((and (< +mid-width+ px +width+) (< +mid-height+ py +height+))
                        (incf q4 count)))))
             positions)
    (* q1 q2 q3 q4)))

(defun elapse (robots seconds)
  (loop with result = (make-hash-table :test #'equal)
        for (px py vx vy) in robots
        for nx = (mod (+ px (* vx seconds)) +width+)
        for ny = (mod (+ py (* vy seconds)) +height+)
        do (incf (gethash (list nx ny) result 0))
        finally (return result)))

(defun day14 (&aux (robots (read-robots)))
  (values (safety-factor (elapse robots 100))
          (dotimes (i (* +width+ +height+))
            (let ((positions (elapse robots i)))
              (when (loop for count being the hash-values of positions always (= count 1))
                (return i))))))

(define-test (= 218433348) (= 6512))
