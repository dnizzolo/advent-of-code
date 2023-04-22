(defpackage :aoc2022.09
  (:documentation "Rope Bridge.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2022.09)

(defun read-motions (&optional (relative-pathname #p"2022/inputs/day09.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (list (a:make-keyword (char line 0)) (parse-integer line :start 2))))))

(defun count-places-seen-by-tail (motions &key (size 2))
  (let ((places (make-hash-table)) (rope (make-array size :initial-element 0)))
    (loop for (direction amount) in motions do
      (loop repeat amount do
        (ecase direction
          (:U (incf (svref rope 0) #c(0 1)))
          (:D (decf (svref rope 0) #c(0 1)))
          (:L (decf (svref rope 0)))
          (:R (incf (svref rope 0))))
        (loop for i from 1 below size for diff = (- (svref rope (1- i)) (svref rope i)) do
          (unless (<= (max (abs (realpart diff)) (abs (imagpart diff))) 1)
            (incf (svref rope i) (complex (a:clamp (realpart diff) -1 1)
                                          (a:clamp (imagpart diff) -1 1)))))
        (setf (gethash (svref rope (1- size)) places) t)))
    (hash-table-count places)))

(defun day09 ()
  (let ((motions (read-motions)))
    (values (count-places-seen-by-tail motions)
            (count-places-seen-by-tail motions :size 10))))

(define-test (= 6269) (= 2557))
