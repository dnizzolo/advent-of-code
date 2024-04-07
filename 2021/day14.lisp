(defpackage :aoc2021.14
  (:documentation "Extended Polymerization.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.14)

(defun read-polymerization (&optional (relative-path #p"2021/inputs/day14.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path))
        (regex (ppcre:create-scanner "([A-Z]+) -> ([A-Z])")))
    (with-open-file (in filename)
      (values (prog1 (coerce (read-line in nil) 'list) (read-line in nil))
              (loop with rules = (make-hash-table :test #'equal)
                    for line = (read-line in nil)
                    while line
                    do (ppcre:register-groups-bind (left right) (regex line)
                         (unless right (error "Expected a valid match."))
                         (let ((key (coerce left 'list))
                               (val (char right 0)))
                           (setf (gethash key rules) val)))
                    finally (return rules))))))

(defun make-adjacent-polymers-hash-table (template &aux (tbl (make-hash-table :test #'equal)))
  (loop for (a b) on template
        while b
        do (incf (gethash (list a b) tbl 0))
        finally (return tbl)))

(defun apply-polymerization-rules (template rules n)
  (loop with adj = (make-adjacent-polymers-hash-table template)
        with counter = (make-counter template)
        repeat n
        do (loop with next-adj = (make-hash-table :test #'equal)
                 for couple being the hash-keys of adj using (hash-value amount)
                 for insert = (gethash couple rules)
                 if insert
                   do (incf (gethash (list (first couple) insert) next-adj 0) amount)
                      (incf (gethash (list insert (second couple)) next-adj 0) amount)
                      (incf (gethash insert counter 0) amount)
                 else
                   do (incf (gethash couple next-adj 0))
                 finally (setf adj next-adj))
        finally (return (- (loop for v being the hash-values of counter maximize v)
                           (loop for v being the hash-values of counter minimize v)))))

(defun day14 ()
  (multiple-value-bind (template rules) (read-polymerization)
    (values
     (apply-polymerization-rules template rules 10)
     (apply-polymerization-rules template rules 40))))

(define-test (= 2587) (= 3318837563123))
