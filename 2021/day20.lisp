(defpackage :aoc2021.20
  (:documentation "Trench Map.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.20)

(defun read-algorithm-and-image (&optional (relative-path #p"2021/inputs/day20.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop with algorithm = (prog1 (read-line in nil) (read-line in nil))
            for line = (read-line in nil)
            while line
            collect line into lines
            finally
               (let ((algorithm (map 'bit-vector
                                     (lambda (c) (if (char= c #\#) 1 0))
                                     algorithm))
                     (image (make-hash-table :test #'equal)))
                 (loop for i from 0
                       for row in lines
                       do (loop for j from 0
                                for char across row
                                do (setf (gethash (cons j i) image)
                                         (if (char= char #\#) 1 0))))
                 (return (values algorithm image)))))))

(defun enhance-image (algorithm image iterations)
  (let ((default 0))
    (dotimes (iteration iterations)
      (declare (ignorable iteration))
      (let ((new-image (make-hash-table :test #'equal)))
        (multiple-value-bind (min-x max-x min-y max-y) (image-bounds image)
          (loop for y from (1- min-y) to (1+ max-y) do
            (loop for x from (1- min-x) to (1+ max-x) do
              (let ((index (neighborhood-index image x y default)))
                (setf (gethash (cons x y) new-image) (aref algorithm index))))))
        (setf image new-image
              default (toggle-default default algorithm)))))
  image)

(defun neighborhood-index (image x y default)
  (let ((bin (make-array 10 :element-type 'bit :fill-pointer 0 :adjustable t)))
    (loop for dy from -1 to 1 do
      (loop for dx from -1 to 1 do
        (let* ((key (cons (+ x dx) (+ y dy)))
               (bit (gethash key image default)))
          (vector-push-extend bit bin))))
    (bit-vector->integer bin)))

(defun image-bounds (image)
  (loop for (x . y) being the hash-key of image
        minimize x into min-x
        maximize x into max-x
        minimize y into min-y
        maximize y into max-y
        finally (return (values min-x max-x min-y max-y))))

(defun count-lit-pixels (image)
  (loop for v being the hash-value of image sum v))

(defun toggle-default (default algorithm)
  (if (= (aref algorithm 0) 1)
      (- 1 default)
      0))

(defun day20 ()
  (multiple-value-bind (algorithm image) (read-algorithm-and-image)
    (values (count-lit-pixels (enhance-image algorithm image 2))
            (count-lit-pixels (enhance-image algorithm image 50)))))

(define-test (= 5432) (= 16016))
