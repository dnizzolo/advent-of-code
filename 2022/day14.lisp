(defpackage :aoc2022.14
  (:documentation "Regolith Reservoir.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.14)

(defun read-cave-structure (&optional (relative-pathname #p"2022/inputs/day14.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
        (cave (make-hash-table :test #'equal)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil) while line do
        (loop for (xi yi xf yf) on (parse-integers-from-string line) by #'cddr
              while yf
              do (cond ((= xi xf)
                        (loop for i from (min yi yf) to (max yi yf) do
                          (setf (gethash (cons xi i) cave) :rock)))
                       ((= yi yf)
                        (loop for i from (min xi xf) to (max xi xf) do
                          (setf (gethash (cons i yi) cave) :rock)))
                       (t (error "Diagonal line detected ~A,~A -> ~A,~A." xi yi xf yf))))))
    (values cave (loop for (_ . y) being the hash-keys of cave maximize y))))

(defun next-grain-position (cave grain &aux (x (car grain)) (y (cdr grain)))
  (let ((possible-positions `((,x . ,(1+ y)) (,(1- x) . ,(1+ y)) (,(1+ x) . ,(1+ y)))))
    (loop for p in possible-positions unless (gethash p cave) return p)))

(defun spawn-grain (cave max-y start-position &key use-floor)
  (loop for prev = nil then curr
        for curr = start-position then next
        for next = (next-grain-position cave curr)
        for (_ . y) = next
        while (if use-floor (and next (< y (+ 2 max-y))) next)
        unless (or use-floor (< y max-y))
          return :off
        finally (setf (gethash curr cave) :sand)
                (return prev)))

(defvar *start-position* '(500 . 0))

(defun solve-part-1 ()
  (multiple-value-bind (cave max-y) (read-cave-structure)
    (loop for i from 0
          for prev = (spawn-grain cave max-y (or prev *start-position*))
          until (eq prev :off)
          finally (return i))))

(defun solve-part-2 ()
  (multiple-value-bind (cave max-y) (read-cave-structure)
    (loop for i from 1
          for prev = (spawn-grain cave max-y (or prev *start-position*) :use-floor t)
          until (gethash *start-position* cave)
          finally (return i))))

(defun day14 ()
  (values (solve-part-1) (solve-part-2)))

(define-test (= 728) (= 27623))
