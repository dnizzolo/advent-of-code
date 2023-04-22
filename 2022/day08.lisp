(defpackage :aoc2022.08
  (:documentation "Treetop Tree House.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.08)

(defun read-tree-grid (&optional (relative-pathname #p"2022/inputs/day08.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
        contents)
    (setf contents
          (with-open-file (in filename)
            (loop for line = (read-line in nil)
                  while line
                  collect (map 'list #'digit-char-p line))))
    (make-array (list (length contents) (length (car contents)))
                :initial-contents contents)))

(defun tree-visible-p (trees i j &aux (tree (aref trees i j)))
  (destructuring-bind (m n) (array-dimensions trees)
    (or
     (loop for k below i always (< (aref trees k j) tree))
     (loop for k below j always (< (aref trees i k) tree))
     (loop for k from (1+ i) below m always (< (aref trees k j) tree))
     (loop for k from (1+ j) below n always (< (aref trees i k) tree)))))

(defun count-visible-trees (trees)
  (destructuring-bind (m n) (array-dimensions trees)
    (loop for i below m sum (loop for j below n count (tree-visible-p trees i j)))))

(defun tree-scenic-score (trees i j &aux (tree (aref trees i j)))
  (destructuring-bind (m n) (array-dimensions trees)
    (let ((u (loop for k from (1- i) downto 0 for steps from 1
                   until (>= (aref trees k j) tree) finally (return steps)))
          (d (loop for k from (1+ i) below m for steps from 1
                   until (>= (aref trees k j) tree) finally (return steps)))
          (l (loop for k from (1- j) downto 0 for steps from 1
                   until (>= (aref trees i k) tree) finally (return steps)))
          (r (loop for k from (1+ j) below n for steps from 1
                   until (>= (aref trees i k) tree) finally (return steps))))
      (* u d l r))))

(defun highest-scenic-score (trees)
  (destructuring-bind (m n) (array-dimensions trees)
    (loop for i below m
          maximize (loop for j below n
                         maximize (tree-scenic-score trees i j)))))

(defun day08 ()
  (let ((grid (read-tree-grid)))
    (values (count-visible-trees grid) (highest-scenic-score grid))))

(define-test (= 1690) (= 535680))
