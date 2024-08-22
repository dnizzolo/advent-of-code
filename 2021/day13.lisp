(defpackage :aoc2021.13
  (:documentation "Transparent Origami.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.13)

(defun read-origami (&optional (relative-path #p"2021/inputs/day13.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path))
        (paper (make-hash-table :test #'equal))
        (insts nil))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while (plusp (length line))
            for (x y) = (parse-integers line)
            do (setf (gethash (list x y) paper) t))
      (setf insts (loop for line = (read-line in nil)
                        while line
                        for coord = (first (parse-integers line))
                        collect (list (if (find #\x line) :left :up) coord))))
    (values paper insts)))

(defun origami-fold (coord paper &key direction)
  (loop for (x y) being the hash-keys of paper
        for diff = (- (if (eq direction :up) y x) coord)
        unless (minusp diff)
          do (setf (gethash (if (eq direction :up)
                                (list x (- coord diff))
                                (list (- coord diff) y))
                            paper)
                   t)
             (remhash (list x y) paper)))

(defun day13/part-1 (paper insts)
  (destructuring-bind (direction coord) (pop insts)
    (origami-fold coord paper :direction direction))
  (hash-table-count paper))

(defun day13/part-2 (paper insts)
  (loop for (direction coord) in insts
        do (origami-fold coord paper :direction direction))
  (let* ((keys (a:hash-table-keys paper))
         (cols (reduce #'max keys :key #'first))
         (rows (reduce #'max keys :key #'second)))
    (with-output-to-string (s)
      (let ((out (make-broadcast-stream s *standard-output*)))
        (terpri out)
        (loop for i upto rows do
          (loop for j upto cols do
            (princ (if (gethash (list j i) paper) #\Full_Block #\.) out))
          (terpri out))))))

(defun day13 ()
  (multiple-value-bind (paper instructions) (read-origami)
    (values (day13/part-1 paper instructions)
            (day13/part-2 paper instructions))))

(define-test
    (= 724)
    (string= "
.██..███....██.███..████.███..█..█.█...
█..█.█..█....█.█..█.█....█..█.█..█.█...
█....█..█....█.███..███..█..█.█..█.█...
█....███.....█.█..█.█....███..█..█.█...
█..█.█....█..█.█..█.█....█.█..█..█.█...
.██..█.....██..███..████.█..█..██..████
"))
