(defpackage :aoc2023.03
  (:documentation "Gear Ratios.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.03)

(defun read-engine-schematic (&optional (relative-pathname #p"2023/inputs/day03.txt"))
  (let* ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
         (contents (uiop:read-file-lines filename)))
    (make-array (list (length contents) (length (first contents)))
                :initial-contents contents)))

(defun adjacent-to-symbol-p (schematic row col)
  (loop for (drow dcol) in '((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 1) (1 -1) (-1 -1))
        for nrow = (+ row drow) for ncol = (+ col dcol)
        thereis (and (array-in-bounds-p schematic nrow ncol)
                     (not (digit-char-p (aref schematic nrow ncol)))
                     (not (char= #\. (aref schematic nrow ncol))))))

(defun sum-part-numbers (schematic)
  (destructuring-bind (rows cols) (array-dimensions schematic)
    (loop for row below rows
          sum (loop with number = 0
                    with adjacent-to-symbol
                    for col below cols
                    for c = (aref schematic row col)
                    when (digit-char-p c)
                      do (setf number (+ (* 10 number) (digit-char-p c))
                               adjacent-to-symbol
                               (or adjacent-to-symbol
                                   (adjacent-to-symbol-p schematic row col)))
                    when (or (not (digit-char-p c))
                             (= col (1- cols)))
                      sum (if adjacent-to-symbol number 0)
                      and do (setf number 0
                                   adjacent-to-symbol nil)))))

(defun compute-gear-ratio (schematic row col &aux (result))
  (destructuring-bind (rows cols) (array-dimensions schematic)
    (declare (ignorable rows))
    (labels ((push-to-result (digits)
               (when digits
                 (push (parse-integer (coerce digits 'string)) result)))
             (digits-right (row col)
               (loop for j from (1+ col) below cols
                     for c = (aref schematic row j)
                     while (digit-char-p c) collect c))
             (digits-left (row col)
               (loop for j downfrom (1- col) to 0
                     for c = (aref schematic row j)
                     while (digit-char-p c) collect c into digits
                     finally (return (nreverse digits))))
             (merge-push-to-result (row col left right)
               (if (and (array-in-bounds-p schematic row col)
                        (digit-char-p (aref schematic row col)))
                   (push-to-result (append left (list (aref schematic row col)) right))
                   (progn (push-to-result left) (push-to-result right)))))
      (let ((west (digits-left row col)) (east (digits-right row col))
            (north-west (digits-left (1- row) col)) (north-east (digits-right (1- row) col))
            (south-west (digits-left (1+ row) col)) (south-east (digits-right (1+ row) col)))
        (push-to-result west)
        (push-to-result east)
        (merge-push-to-result (1- row) col north-west north-east)
        (merge-push-to-result (1+ row) col south-west south-east)
        (when (= 2 (length result))
          (* (first result) (second result)))))))

(defun sum-gear-ratios (schematic &aux (result 0))
  (destructuring-bind (rows cols) (array-dimensions schematic)
    (dotimes (row rows result)
      (dotimes (col cols)
        (when (char= #\* (aref schematic row col))
          (let ((gear-ratio (compute-gear-ratio schematic row col)))
            (when gear-ratio (incf result gear-ratio))))))))

(defun day03 (&aux (schematic (read-engine-schematic)))
  (values (sum-part-numbers schematic) (sum-gear-ratios schematic)))

(define-test (= 521601) (= 80694070))
