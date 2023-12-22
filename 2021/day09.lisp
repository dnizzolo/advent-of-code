(defpackage :aoc2021.09
  (:documentation "Smoke Basin.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.09)

(defun read-caves (&optional (rel-path #p"2021/inputs/day09.txt"))
  (let* ((filename (asdf:system-relative-pathname :advent-of-code rel-path))
         (lines (with-open-file (in filename)
                  (loop for line = (read-line in nil)
                        while line
                        collect (map 'list #'digit-char-p line)))))
    (make-array (list (length lines) (length (car lines)))
                :initial-contents lines)))

(defun point-neighbours (grid i j &aux neighs)
  (declare (type (integer 0) i j))
  (destructuring-bind (rows cols) (array-dimensions grid)
    (loop for (di dj) in '((1 0) (0 1) (-1 0) (0 -1))
          for ni = (+ i di)
          for nj = (+ j dj)
          when (and (< -1 ni rows) (< -1 nj cols))
            do (push (list ni nj) neighs)
          finally (return neighs))))

(defun low-point-p (grid i j)
  (loop with point = (aref grid i j)
        for neigh in (point-neighbours grid i j)
        always (< point (apply #'aref grid neigh))))

(defun low-points (grid)
  (destructuring-bind (rows cols) (array-dimensions grid)
    (loop for i below rows
          nconc (loop for j below cols
                      when (low-point-p grid i j)
                        collect (list i j)))))

(defun day09/part-1 (caves)
  (loop for (i j) in (low-points caves) sum (1+ (aref caves i j))))

(defun basin (caves i j &aux (seen (make-hash-table :test #'equal)))
  (labels ((visit (x y &aux (curr (aref caves x y)))
             (setf (gethash (list x y) seen) t)
             (loop for (ni nj) in (remove-if (lambda (n)
                                               (gethash n seen))
                                             (point-neighbours caves x y))
                   for neigh = (aref caves ni nj)
                   when (< curr neigh 9)
                     do (visit ni nj))))
    (visit i j))
  (a:hash-table-keys seen))

(defun day09/part-2 (caves)
  (let* ((sizes (loop for (i j) in (low-points caves)
                      collect (length (basin caves i j))))
         (sorted (sort sizes #'>)))
    (* (first sorted) (second sorted) (third sorted))))

(defun day09 ()
  (let ((caves (read-caves)))
    (values (day09/part-1 caves) (day09/part-2 caves))))

(define-test (= 498) (= 1071000))
