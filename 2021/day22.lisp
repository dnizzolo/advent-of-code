(defpackage :aoc2021.22
  (:documentation "Reactor Reboot.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.22)

(defstruct (cuboid (:constructor %make-cuboid) (:conc-name nil))
  x-low x-high
  y-low y-high
  z-low z-high)

(defun make-cuboid (x-low x-high y-low y-high z-low z-high)
  (%make-cuboid
   :x-low x-low :x-high x-high
   :y-low y-low :y-high y-high
   :z-low z-low :z-high z-high))

(defun cuboid-volume (cuboid)
  (* (1+ (- (x-high cuboid) (x-low cuboid)))
     (1+ (- (y-high cuboid) (y-low cuboid)))
     (1+ (- (z-high cuboid) (z-low cuboid)))))

(defun cuboid-intersect (a b)
  (let ((x-low (max (x-low a) (x-low b))) (x-high (min (x-high a) (x-high b)))
        (y-low (max (y-low a) (y-low b))) (y-high (min (y-high a) (y-high b)))
        (z-low (max (z-low a) (z-low b))) (z-high (min (z-high a) (z-high b))))
    (when (and (<= x-low x-high) (<= y-low y-high) (<= z-low z-high))
      (make-cuboid x-low x-high y-low y-high z-low z-high))))

(defun cuboid-subtract (a b &aux (i (cuboid-intersect a b)))
  (if (null i)
      (list a)
      (let (result
            (xl (x-low a)) (xh (x-high a))
            (yl (y-low a)) (yh (y-high a))
            (zl (z-low a)) (zh (z-high a))
            (ixl (x-low i)) (ixh (x-high i))
            (iyl (y-low i)) (iyh (y-high i))
            (izl (z-low i)) (izh (z-high i)))
        (when (< xl ixl)
          (push (make-cuboid xl (1- ixl) yl yh zl zh) result))
        (when (< ixh xh)
          (push (make-cuboid (1+ ixh) xh yl yh zl zh) result))
        (when (< yl iyl)
          (push (make-cuboid ixl ixh yl (1- iyl) zl zh) result))
        (when (< iyh yh)
          (push (make-cuboid ixl ixh (1+ iyh) yh zl zh) result))
        (when (< zl izl)
          (push (make-cuboid ixl ixh iyl iyh zl (1- izl)) result))
        (when (< izh zh)
          (push (make-cuboid ixl ixh iyl iyh (1+ izh) zh) result))
        result)))

(defun read-reboot-steps (&optional (relative-path #p"2021/inputs/day22.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (list (char= (char line 1) #\n)
                          (apply #'make-cuboid (parse-integers line)))))))

(defun initialization-procedure (steps)
  (flet ((lower-bound (coordinate) (max -50 coordinate))
         (higher-bound (coordinate) (min 50 coordinate)))
    (loop with on-cuboids = (make-hash-table :test #'equal)
          for (mode cuboid) in steps
          do (let ((x-lower (lower-bound (x-low cuboid)))
                   (x-higher (higher-bound (x-high cuboid)))
                   (y-lower (lower-bound (y-low cuboid)))
                   (y-higher (higher-bound (y-high cuboid)))
                   (z-lower (lower-bound (z-low cuboid)))
                   (z-higher (higher-bound (z-high cuboid))))
               (loop for x from x-lower to x-higher do
                 (loop for y from y-lower to y-higher do
                   (loop for z from z-lower to z-higher do
                     (let ((key (list x y z)))
                       (if mode
                           (setf (gethash key on-cuboids) t)
                           (remhash key on-cuboids)))))))
          finally (return (hash-table-count on-cuboids)))))

(defun full-procedure (steps)
  (loop with on-cuboids
        for (mode cuboid) in steps
        for updated = (loop for on-cuboid in on-cuboids
                            nconc (cuboid-subtract on-cuboid cuboid))
        do (setf on-cuboids updated)
           (when mode (push cuboid on-cuboids))
        finally (return (loop for cuboid in on-cuboids sum (cuboid-volume cuboid)))))

(defun day22 (&aux (steps (read-reboot-steps)))
  (values (initialization-procedure steps)
          (full-procedure steps)))

(define-test (= 655005) (= 1125649856443608))
