(defpackage :aoc2021.12
  (:documentation "Passage Pathing.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.12)

(defun read-caves-graph (&optional (relative-path #p"2021/inputs/day12.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path))
        (adj-list (make-hash-table)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for (v1 v2) = (uiop:split-string line :separator '(#\-))
            for sv1 = (intern v1 :keyword) and sv2 = (intern v2 :keyword)
            do (push sv2 (gethash sv1 adj-list)) (push sv1 (gethash sv2 adj-list))))
    adj-list))

(defun small-cave-p (cave) (lower-case-p (char (string cave) 0)))

(defun day12/part-1 (adj-list &aux (start :|start|) (goal :|end|) path)
  (labels ((count-paths (source)
             (cond ((eq source goal) 1)
                   ((eq source start) 0)
                   ((and (small-cave-p source) (member source path)) 0)
                   (t (push source path)
                      (loop for adj in (gethash source adj-list)
                            sum (count-paths adj)
                            finally (pop path))))))
    (loop for adj in (gethash start adj-list) sum (count-paths adj))))

(defun day12/part-2 (adj-list &aux (start :|start|) (goal :|end|) path)
  (labels ((count-paths (source twice-in-path-p)
             (let ((already-in-path-p (and (small-cave-p source) (member source path))))
               (cond ((eq source goal) 1)
                     ((eq source start) 0)
                     ((and already-in-path-p twice-in-path-p) 0)
                     (t (push source path)
                        (loop for adj in (gethash source adj-list)
                              sum (count-paths adj (or already-in-path-p twice-in-path-p))
                              finally (pop path)))))))
    (loop for adj in (gethash start adj-list) sum (count-paths adj nil))))

(defun day12 (&aux (adj-list (read-caves-graph)))
  (values (day12/part-1 adj-list) (day12/part-2 adj-list)))

(define-test (= 4011) (= 108035))
