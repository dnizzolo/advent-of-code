(defpackage :aoc2021.17
  (:documentation "Trick Shot.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.17)

(defstruct (target-area (:constructor %make-target-area) (:conc-name nil))
  x-min x-max
  y-min y-max)

(defun make-target-area (x1 x2 y1 y2)
  (%make-target-area :x-min (min x1 x2) :x-max (max x1 x2)
                     :y-min (min y1 y2) :y-max (max y1 y2)))

(defun read-target-area (&optional (relative-pathname #p"2021/inputs/day17.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (apply #'make-target-area
           (parse-integers (uiop:read-file-string filename)))))

(defstruct (probe (:conc-name nil))
  (x-position 0) (y-position 0)
  x-velocity y-velocity)

(defun on-target-area-p (target-area probe)
  (and (<= (x-min target-area) (x-position probe) (x-max target-area))
       (<= (y-min target-area) (y-position probe) (y-max target-area))))

(defun overshotp (target-area probe)
  (or (> (x-position probe) (x-max target-area))
      (< (y-position probe) (y-min target-area))))

(defun move (probe)
  (with-slots (x-position y-position x-velocity y-velocity) probe
    (incf x-position x-velocity)
    (incf y-position y-velocity)
    (cond ((plusp x-velocity) (decf x-velocity))
          ((minusp x-velocity) (incf x-velocity)))
    (decf y-velocity)))

(defun shoot (target-area x-velocity y-velocity)
  (loop with probe = (make-probe :x-velocity x-velocity :y-velocity y-velocity)
        until (overshotp target-area probe)
        when (on-target-area-p target-area probe)
          return y-max
        do (move probe)
        maximize (y-position probe) into y-max))

(defun min-x-velocity (target-area)
  (loop for v from 0
        when (>= (triangular v) (x-min target-area)) return v))

(defun max-x-velocity (target-area)
  (x-max target-area))

(defun min-y-velocity (target-area)
  (y-min target-area))

(defun max-y-velocity (target-area)
  (- (y-min target-area)))

(defun day17 (&aux (target-area (read-target-area)) (part-1 0) (part-2 0))
  (loop for xv from (min-x-velocity target-area) to (max-x-velocity target-area) do
    (loop for yv from (min-y-velocity target-area) to (max-y-velocity target-area) do
      (a:when-let ((best (shoot target-area xv yv)))
        (a:maxf part-1 best)
        (incf part-2))))
  (values part-1 part-2))

(define-test (= 8646) (= 5945))
