(defpackage :aoc2023.15
  (:documentation "Lens Library.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.15)

(defun read-initialization-sequence (&optional (relative-pathname #p"2023/inputs/day15.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (uiop:split-string (uiop:read-file-line filename) :separator '(#\,))))

(defun hash (string)
  (loop with result = 0
        for char across string
        do (setf result (mod (* 17 (+ result (char-code char))) 256))
        finally (return result)))

(defun hashmap (init-seq)
  (loop with boxes = (make-array 256 :initial-element nil)
        for s in init-seq
        for p= = (position #\= s)
        for p- = (position #\- s)
        for v = (make-array (or p= p-) :displaced-to s :element-type 'character)
        for i = (hash v)
        for ls = (svref boxes i)
        do (cond (p- (setf (svref boxes i)
                           (delete-if (lambda (c) (string= (car c) v)) ls)))
                 (p= (let ((focal-length (digit-char-p (char s (1+ p=))))
                           (pair (assoc v ls :test #'string=)))
                       (if pair
                           (rplacd pair focal-length)
                           (setf (svref boxes i)
                                 (nconc ls (list (cons v focal-length))))))))
        finally (return (loop for i from 1
                              for ls across boxes
                              sum (loop for j from 1
                                        for (s . f) in ls
                                        sum (* i j f))))))

(defun day15 (&aux (init-seq (read-initialization-sequence)))
  (values (loop for s in init-seq sum (hash s)) (hashmap init-seq)))

(define-test (= 511343) (= 294474))
