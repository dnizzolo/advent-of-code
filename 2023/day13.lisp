(defpackage :aoc2023.13
  (:documentation "Point of Incidence.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.13)

(defun read-patterns (&optional (relative-pathname #p"2023/inputs/day13.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop while (listen in)
            collect (loop for line = (read-line in nil)
                          while (plusp (length line))
                          collect line into rows
                          finally (return (pattern-to-rows-cols
                                           (make-array
                                            (list (length rows) (length (first rows)))
                                            :initial-contents rows))))))))

(defun pattern-to-rows-cols (pattern)
  (destructuring-bind (n m) (array-dimensions pattern)
    (flet ((rows-as-ints ()
             (let ((result (make-array n)))
               (dotimes (i n result)
                 (let ((acc 0))
                   (dotimes (j m acc)
                     (setf acc (+ (* 2 acc) (if (char= #\. (aref pattern i j)) 0 1))))
                   (setf (aref result i) acc)))))
           (cols-as-ints ()
             (let ((result (make-array m)))
               (dotimes (j m result)
                 (let ((acc 0))
                   (dotimes (i n acc)
                     (setf acc (+ (* 2 acc) (if (char= #\. (aref pattern i j)) 0 1))))
                   (setf (aref result j) acc))))))
      (cons (rows-as-ints) (cols-as-ints)))))

(defun find-reflection (ns &aux (n (length ns)))
  (loop for try below (1- n)
        when (loop for i from try downto 0
                   for j from (1+ try) below n
                   always (= (svref ns i) (svref ns j)))
          return (1+ try)))

(defun find-smudge (ns &aux (n (length ns)))
  (loop for try below (1- n)
        when (loop with counter = 0
                   for i from try downto 0
                   for j from (1+ try) below n
                   for diff = (logcount (logxor (svref ns i) (svref ns j)))
                   while (<= diff 1)
                   when (= diff 1)
                     do (incf counter)
                   finally (return (and (or (= i -1) (= j n))
                                        (= 1 counter))))
          return (1+ try)))

(defun day13 (&aux (patterns (read-patterns)))
  (values (loop for (rows . cols) in patterns
                for h = (or (find-reflection rows) 0)
                for v = (or (find-reflection cols) 0)
                sum (+ v (* h 100)))
          (loop for (rows . cols) in patterns
                for h = (or (find-smudge rows) 0)
                for v = (or (find-smudge cols) 0)
                sum (+ v (* h 100)))))

(define-test (= 31956) (= 37617))
