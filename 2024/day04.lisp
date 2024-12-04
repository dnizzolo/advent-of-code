(defpackage :aoc2024.04
  (:documentation "Ceres Search.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.04)

(defun read-word-search (&optional (relative-pathname #p"2024/inputs/day04.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (coerce line 'list) into lines
            finally (return (make-array (list (length lines) (length (first lines)))
                                        :initial-contents lines))))))

(defun count-xmas (word-search &aux (result 0))
  (destructuring-bind (rows cols) (array-dimensions word-search)
    (dotimes (i rows result)
      (dotimes (j cols)
        (loop for deltas in '(((0 1) (0 2) (0 3))
                              ((0 -1) (0 -2) (0 -3))
                              ((1 0) (2 0) (3 0))
                              ((-1 0) (-2 0) (-3 0))
                              ((1 1) (2 2) (3 3))
                              ((-1 -1) (-2 -2) (-3 -3))
                              ((1 -1) (2 -2) (3 -3))
                              ((-1 1) (-2 2) (-3 3)))
              for neighs = (loop for (di dj) in deltas collect (list (+ i di) (+ j dj)))
              when (and
                    (loop for (ni nj) in neighs always (array-in-bounds-p word-search ni nj))
                    (equal '(#\X #\M #\A #\S)
                           (cons
                            (aref word-search i j)
                            (loop for (ni nj) in neighs collect (aref word-search ni nj)))))
                do (incf result))))))

(defun count-x-mas (word-search &aux (result 0))
  (destructuring-bind (rows cols) (array-dimensions word-search)
    (dotimes (i rows result)
      (dotimes (j cols)
        (let* ((deltas '((-1 1) (1 1) (1 -1) (-1 -1)))
               (neighs (loop for (di dj) in deltas collect (list (+ i di) (+ j dj)))))
          (when (and
                 (char= #\A (aref word-search i j))
                 (loop for (ni nj) in neighs always (array-in-bounds-p word-search ni nj))
                 (member
                  (loop for (ni nj) in neighs collect (aref word-search ni nj))
                  '((#\M #\M #\S #\S) (#\M #\S #\S #\M) (#\S #\S #\M #\M) (#\S #\M #\M #\S))
                  :test #'equal))
            (incf result)))))))

(defun day04 (&aux (word-search (read-word-search)))
  (values (count-xmas word-search) (count-x-mas word-search)))

(define-test (= 2633) (= 1936))
