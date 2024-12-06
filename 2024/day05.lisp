(defpackage :aoc2024.05
  (:documentation "Print Queue.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.05)

(defun read-pages (&optional (relative-pathname #p"2024/inputs/day05.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with before = (make-hash-table)
            initially (loop for line = (read-line in nil)
                            while (plusp (length line))
                            for (left right) = (parse-integers line)
                            do (push left (gethash right before nil)))
            for line = (read-line in nil)
            while line
            collect (coerce (parse-integers line) 'vector) into updates
            finally (return (values before (coerce updates 'vector)))))))

(defun correct-update-p (before update &aux (length (length update)))
  (loop for i below length
        do (loop for j from (1+ i) below length
                 when (member (aref update j) (gethash (aref update i) before))
                   do (return-from correct-update-p nil))
        finally (return t)))

(defun day05 ()
  (multiple-value-bind (before updates) (read-pages)
    (loop for update across updates
          if (correct-update-p before update)
            sum (aref update (floor (length update) 2)) into part-1
          else
            sum (aref (sort update (lambda (a b) (member a (gethash b before))))
                      (floor (length update) 2))
              into part-2
          finally (return (values part-1 part-2)))))

(define-test (= 7198) (= 4230))
