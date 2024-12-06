(defpackage :aoc2024.06
  (:documentation "Guard Gallivant.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.06)

(defun read-map (&optional (relative-pathname #p"2024/inputs/day06.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect line into lines
            finally (return (make-array (list (length lines) (length (first lines)))
                                        :initial-contents lines))))))

(defun next-direction (direction)
  (ecase direction
    (:up :right)
    (:right :down)
    (:down :left)
    (:left :up)))

(defun next-position (i j direction)
  (ecase direction
    (:up (values (1- i) j))
    (:right (values i (1+ j)))
    (:down (values (1+ i) j))
    (:left (values i (1- j)))))

(defun reached-positions (map i j &aux (direction :up) (seen (make-hash-table :test #'equal)))
  (loop
    (setf (gethash (cons i j) seen) t)
    (multiple-value-bind (ni nj) (next-position i j direction)
      (cond ((not (array-in-bounds-p map ni nj))
             (return seen))
            ((char= #\# (aref map ni nj))
             (setf direction (next-direction direction)))
            (t (setf i ni j nj))))))

(defun try-obstruction (map i j obstruction-i obstruction-j
                        &aux (direction :up) (seen (make-hash-table :test #'equal)))
  (setf (aref map obstruction-i obstruction-j) #\#)
  (prog1
      (loop
        (when (gethash (list i j direction) seen) (return t))
        (setf (gethash (list i j direction) seen) t)
        (multiple-value-bind (ni nj) (next-position i j direction)
          (cond ((not (array-in-bounds-p map ni nj))
                 (return nil))
                ((char= #\# (aref map ni nj))
                 (setf direction (next-direction direction)))
                (t (setf i ni j nj)))))
    (setf (aref map obstruction-i obstruction-j) #\.)))

(defun day06 (&aux (map (read-map)))
  (multiple-value-bind (guard-i guard-j) (position-2d #\^ map)
    (let ((positions (reached-positions map guard-i guard-j)))
      (values (hash-table-count positions)
              (loop for (obstruction-i . obstruction-j) being the hash-keys of positions
                    unless (and (= obstruction-i guard-i) (= obstruction-j guard-j))
                      count (try-obstruction
                             map
                             guard-i
                             guard-j
                             obstruction-i
                             obstruction-j))))))

(define-test (= 4988) (= 1697))
