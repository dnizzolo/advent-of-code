(defpackage #:aoc2025.07
  (:documentation "Laboratories.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.07)

(defun read-diagram (&optional (relative-pathname #p"2025/inputs/day07.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with length
            for line = (read-line in nil)
            while line
            unless length
              do (setf length (length line))
            collect (loop for c across line
                          for i from 0
                          if (member c '(#\S #\^))
                            collect i)
              into positions
            finally (return (values length (first (first positions)) (rest positions)))))))

(defun count-splits (length start splitters)
  (loop with count = 0
        with set = (make-array length :element-type 'bit)
        initially (setf (bit set start) 1)
        for splits in splitters
        do (loop for s in splits
                 when (= (bit set s) 1)
                   do (incf count)
                      (setf (bit set s) 0
                            (bit set (1- s)) 1
                            (bit set (1+ s)) 1))
        finally (return count)))

(defun count-timelines (length start splitters)
  (loop with counts = (make-array length)
        initially (setf (aref counts start) 1)
        for splits in splitters
        do (loop for s in splits
                 do (incf (aref counts (1- s)) (aref counts s))
                    (incf (aref counts (1+ s)) (aref counts s))
                    (setf (aref counts s) 0))
        finally (return (reduce #'+ counts))))

(defun day07 ()
  (multiple-value-bind (length start splitters) (read-diagram)
    (values (count-splits length start splitters)
            (count-timelines length start splitters))))

(define-test (= 1635) (= 58097428661390))
