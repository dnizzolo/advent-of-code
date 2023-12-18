(defpackage :aoc2023.10
  (:documentation "Pipe Maze.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2023.10)

(defun read-diagram (&optional (relative-pathname #p"2023/inputs/day10.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line collect line into lines
            finally (return (make-array (list (length lines) (length (first lines)))
                                        :initial-contents lines))))))

(defun adjacent (diagram i j &aux (char (aref diagram i j)))
  (remove-if-not
   (lambda (c) (array-in-bounds-p diagram (car c) (cdr c)))
   (ecase char
     (#\| (list (cons (1- i) j) (cons (1+ i) j)))
     (#\- (list (cons i (1- j)) (cons i (1+ j))))
     (#\L (list (cons (1- i) j) (cons i (1+ j))))
     (#\J (list (cons (1- i) j) (cons i (1- j))))
     (#\7 (list (cons (1+ i) j) (cons i (1- j))))
     (#\F (list (cons (1+ i) j) (cons i (1+ j))))
     (#\. nil)
     (#\S (let (result
                (north (and (array-in-bounds-p diagram (1- i) j) (aref diagram (1- i) j)))
                (east (and (array-in-bounds-p diagram i (1+ j)) (aref diagram i (1+ j))))
                (south (and (array-in-bounds-p diagram (1+ i) j) (aref diagram (1+ i) j)))
                (west (and (array-in-bounds-p diagram i (1- j)) (aref diagram i (1- j)))))
            (when (member north '(#\| #\7 #\F)) (push (cons (1- i) j) result))
            (when (member east '(#\- #\J #\7)) (push (cons i (1+ j)) result))
            (when (member south '(#\| #\L #\J)) (push (cons (1+ i) j) result))
            (when (member west '(#\- #\F #\L)) (push (cons i (1- j)) result))
            result)))))

(defun main-loop (diagram)
  (multiple-value-bind (srow scol)
      (position-2d #\S diagram)
    (loop with s = (cons srow scol)
          with seen = (make-hash-table :test #'equal)
          with curr = (first (adjacent diagram srow scol))
          with adjacents = (remove s (adjacent diagram (car curr) (cdr curr)) :test #'equal)
          until (equal curr s)
          collect curr into cycle
          do (setf (gethash curr seen) t
                   curr (loop for n in adjacents unless (gethash n seen) return n)
                   adjacents (adjacent diagram (car curr) (cdr curr)))
          finally (return (cons s cycle)))))

(defun enclosed-area (curve)
  (loop for (a b) on curve
        while b
        sum (* (cdr b) (- (car b) (car a))) into integral
        finally
           (let ((a (a:lastcar curve)) (b (first curve)))
             (incf integral (* (cdr b) (- (car b) (car a))))
             (return (- (abs integral) (ash (length curve) -1) -1)))))

(defun day10 (&aux (diagram (read-diagram)) (main-loop (main-loop diagram)))
  (values (ash (length main-loop) -1) (enclosed-area main-loop)))

(define-test (= 6979) (= 443))
