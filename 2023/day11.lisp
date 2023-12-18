(defpackage :aoc2023.11
  (:documentation "Cosmic Expansion.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2023.11)

(defun read-galaxies (&optional (relative-pathname #p"2023/inputs/day11.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line collect line into lines
            finally (return (make-array (list (length lines) (length (first lines)))
                                        :initial-contents lines))))))

(defun galaxies-positions (galaxies &aux result)
  (destructuring-bind (rows cols)
      (array-dimensions galaxies)
    (dotimes (i rows result)
      (dotimes (j cols)
        (when (char= #\# (aref galaxies i j))
          (push (cons i j) result))))))

(defun manhattan-distance (x y)
  (+ (abs (- (car x) (car y))) (abs (- (cdr x) (cdr y)))))

(defun empty-rows (galaxies)
  (destructuring-bind (rows cols)
      (array-dimensions galaxies)
    (loop for i below rows
          when (loop for j below cols always (char= #\. (aref galaxies i j)))
            collect i)))

(defun empty-cols (galaxies)
  (destructuring-bind (rows cols)
      (array-dimensions galaxies)
    (loop for j below cols
          when (loop for i below rows always (char= #\. (aref galaxies i j)))
            collect j)))

(defun sum-distances (positions empty-rows empty-cols &key (weight 2) &aux (total 0))
  (a:map-combinations
   (lambda (c)
     (destructuring-bind (l r) c
       (let ((min-row (min (car l) (car r))) (max-row (max (car l) (car r)))
             (min-col (min (cdr l) (cdr r))) (max-col (max (cdr l) (cdr r))))
         (incf total (+ (manhattan-distance l r)
                        (* (1- weight) (count-if (lambda (e) (< min-row e max-row))
                                                 empty-rows))
                        (* (1- weight) (count-if (lambda (e) (< min-col e max-col))
                                                 empty-cols)))))))
   positions
   :length 2
   :copy nil)
  total)

(defun day11 ()
  (let* ((galaxies (read-galaxies))
         (positions (galaxies-positions galaxies))
         (empty-rows (empty-rows galaxies))
         (empty-cols (empty-cols galaxies)))
    (values (sum-distances positions empty-rows empty-cols)
            (sum-distances positions empty-rows empty-cols :weight 1000000))))

(define-test (= 10228230) (= 447073334102))
