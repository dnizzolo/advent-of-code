(defpackage :aoc2024.19
  (:documentation "Linen Layout.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.19)

(defun read-patterns-and-designs (&optional (relative-pathname #p"2024/inputs/day19.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with patterns = (prog1 (read-line in nil) (read-line in nil))
            with patterns-table = (make-hash-table :test #'equal)
            for line = (read-line in nil)
            while line
            collect line into designs
            finally (loop for pattern in (uiop:split-string (delete #\Space patterns)
                                                            :separator '(#\,))
                          do (setf (gethash pattern patterns-table) t))
                    (return (values patterns-table designs))))))

(defvar *patterns*)

(define-memo-function (obtainable-p (design))
  (let ((length (length design)))
    (if (zerop length)
        t
        (loop for i upto length
              when (and (gethash (subseq design 0 i) *patterns*)
                        (obtainable-p (subseq design i)))
                return t))))

(define-memo-function (ways-to-obtain (design))
  (let ((length (length design)))
    (if (zerop length)
        1
        (loop with acc = 0
              for i upto length
              when (gethash (subseq design 0 i) *patterns*)
                do (incf acc (ways-to-obtain (subseq design i)))
              finally (return acc)))))

(defun day19 ()
  (multiple-value-bind (patterns designs) (read-patterns-and-designs)
    (let ((*patterns* patterns))
      (values (count-if #'obtainable-p designs)
              (reduce #'+ designs :key #'ways-to-obtain)))))

(define-test (= 347) (= 919219286602165))
