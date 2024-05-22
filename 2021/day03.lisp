(defpackage :aoc2021.03
  (:documentation "Binary Diagnostic.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.03)

(defun read-diagnostic-report (path)
  (mapcar #'string->bit-vector
          (uiop:read-file-lines
           (asdf:system-relative-pathname :advent-of-code path))))

(defun string->bit-vector (s)
  (make-array (length s) :element-type 'bit :initial-contents (map 'list #'digit-char-p s)))

(defun day03/part-1 (diagnostics)
  (let* ((gamma (apply #'map
                       'bit-vector
                       (lambda (&rest bits) (if (< (count 0 bits) (count 1 bits)) 0 1))
                       diagnostics))
         (epsilon (bit-not gamma)))
    (* (bit-vector->integer gamma) (bit-vector->integer epsilon))))

(defun most-common-nth-bit (i bvs)
  (if (>= (count 1 (mapcar (lambda (bv) (sbit bv i)) bvs)) (/ (length bvs) 2)) 1 0))

(defun frequency-based-filter (i bvs &key keep-when)
  (if (null (cdr bvs))
      (car bvs)
      (let ((most-common (most-common-nth-bit i bvs)))
        (frequency-based-filter (1+ i)
                                (remove-if-not
                                 (lambda (bv)
                                   (funcall keep-when
                                            (sbit bv i)
                                            most-common))
                                 bvs)
                                :keep-when keep-when))))

(defun find-oxygen-generator-rating (diagnostics)
  (frequency-based-filter 0 diagnostics :keep-when #'=))

(defun find-carbon-dioxide-scrubber-rating (diagnostics)
  (frequency-based-filter 0 diagnostics :keep-when #'/=))

(defun day03/part-2 (diagnostics)
  (* (bit-vector->integer (find-oxygen-generator-rating diagnostics))
     (bit-vector->integer (find-carbon-dioxide-scrubber-rating diagnostics))))

(defun day03 ()
  (let ((diagns (read-diagnostic-report #p"2021/inputs/day03.txt")))
    (values (day03/part-1 diagns) (day03/part-2 diagns))))

(define-test (= 845186) (= 4636702))
