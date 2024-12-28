(defpackage :aoc2024.13
  (:documentation "Claw Contraption.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.13)

(defun read-linear-systems (&optional (relative-pathname #p"2024/inputs/day13.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
        (systems nil))
    (with-open-file (in filename)
      (loop
        (let ((line-1 (read-line in nil))
              (line-2 (read-line in nil))
              (line-3 (read-line in nil)))
          (unless line-1 (return systems))
          (push (append (parse-integers line-1)
                        (parse-integers line-2)
                        (parse-integers line-3))
                systems)
          (read-line in nil))))))

(defun solve-linear-system (ax ay bx by px py)
  (let ((det (- (* ax by) (* bx ay))))
    (unless (zerop det)
      (let* ((a (/ (- (* px by) (* bx py)) det))
             (b (/ (- px (* ax a)) bx)))
        (when (and (integerp a) (integerp b))
          (values a b))))))

(defun day13 (&aux (systems (read-linear-systems)))
  (loop with part-1 = 0
        with part-2 = 0
        for (ax ay bx by px py) in systems
        do (multiple-value-bind (a b) (solve-linear-system ax ay bx by px py)
             (when (and a b (<= 0 a 100) (<= 0 b 100))
               (incf part-1 (+ (* 3 a) b))))
           (multiple-value-bind (a b)
               (solve-linear-system
                ax ay bx by (+ px 10000000000000) (+ py 10000000000000))
             (when (and a b)
               (incf part-2 (+ (* 3 a) b))))
        finally (return (values part-1 part-2))))

(define-test (= 35574) (= 80882098756071))
