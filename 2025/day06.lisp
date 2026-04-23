(defpackage #:aoc2025.06
  (:documentation "Trash Compactor.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.06)

(defun read-problems (&optional (relative-pathname #p"2025/inputs/day06.txt"))
  (let* ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
         (lines (uiop:read-file-lines filename))
         (nums (loop with numlines = (butlast lines)
                     with max = (reduce #'max numlines :key #'length)
                     for line in numlines
                     collect (format nil "~va" max line)))
         (ops (loop for ch across (first (last lines))
                    when (char= ch #\+)
                      collect #'+
                    when (char= ch #\*)
                      collect #'*)))
    (values
     (make-array (list (length nums) (length (first nums)))
                 :initial-contents nums)
     ops)))

(defun solve-1 (operations numbers)
  (let* ((rows (mapcar (lambda (r)
                         (parse-all-integers (coerce r 'string)))
                       (2d-array->list numbers)))
         (cols (transpose rows)))
    (loop for op in operations
          for col in cols
          sum (reduce op col))))

(defun solve-2 (operations numbers)
  (destructuring-bind (rows cols) (array-dimensions numbers)
    (loop with total = 0
          with ops = operations
          with acc
          for j below cols
          do (let ((str (with-output-to-string (out)
                          (loop for i below rows
                                for c = (aref numbers i j)
                                when (digit-char-p c)
                                  do (write-char c out)))))
               (if (zerop (length str))
                   (progn
                     (incf total (reduce (first ops) acc))
                     (setf acc nil ops (rest ops)))
                   (push (parse-integer str) acc)))
          finally (return (+ total (reduce (first ops) acc))))))

(defun day06 ()
  (multiple-value-bind (nums ops) (read-problems)
    (values (solve-1 ops nums) (solve-2 ops nums))))

(define-test (= 5667835681547) (= 9434900032651))
