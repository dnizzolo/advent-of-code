(defpackage :aoc2023.16
  (:documentation "The Floor Will Be Lava.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.16)

(defun read-cavern (&optional (relative-pathname #p"2023/inputs/day16.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (let ((lines (uiop:read-file-lines filename)))
      (make-array (list (length lines) (length (first lines)))
                  :initial-contents lines :element-type 'character))))

;; 0 -> north.
;; 1 -> south.
;; 2 -> west.
;; 3 -> east.
(defun tick (i j dir)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (declare (type (integer 0 3) dir))
  (declare (type (integer 0 255) i j))
  (let ((r (svref #(-1 1 0 0) dir))
        (c (svref #(0 0 -1 1) dir)))
    (list (+ i r) (+ j c) dir)))

(defun adjacent (cavern i j dir
                 &aux
                   (char (aref cavern i j))
                   (n (array-dimension cavern 0))
                   (m (array-dimension cavern 1)))
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (declare (type fixnum i j dir n m))
  (declare (type (simple-array character (* *)) cavern))
  (remove-if-not
   (lambda (c) (and (< -1 (first c) n) (< -1 (second c) m)))
   (ecase char
     (#\. (list (tick i j dir)))
     (#\| (if (or (= dir 0) (= dir 1))
              (list (tick i j dir))
              (list (tick i j 0) (tick i j 1))))
     (#\- (if (or (= dir 2) (= dir 3))
              (list (tick i j dir))
              (list (tick i j 2) (tick i j 3))))
     (#\\ (list (tick i j (svref #(2 3 0 1) dir))))
     (#\/ (list (tick i j (svref #(3 2 1 0) dir)))))))

(defun count-energized (cavern i j dir)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (loop with curr = (list (list i j dir))
        with seen = (make-hash-table :test #'equal)
        while curr
        do (loop with next
                 for cur in curr
                 for (x y d) = cur
                 do (setf (gethash cur seen) t)
                    (loop for p in (adjacent cavern x y d)
                          unless (gethash p seen)
                            do (push p next))
                 finally (setf curr next))
        finally (return
                  (let ((coun (make-hash-table :test #'equal)))
                    (maphash (lambda (k v)
                               (declare (ignorable v))
                               (setf (gethash (list (first k) (second k)) coun) t))
                             seen)
                    (hash-table-count coun)))))

(defun day16 (&aux (cavern (read-cavern)))
  (values (count-energized cavern 0 0 3)
          (destructuring-bind (n m) (array-dimensions cavern)
            (max (loop for i below n
                       maximize (max (count-energized cavern i 0 3)
                                     (count-energized cavern i (1- m) 2)))
                 (loop for j below m
                       maximize (max (count-energized cavern 0 j 1)
                                     (count-energized cavern (1- n) j 0)))))))

(define-test (= 8323) (= 8491))
