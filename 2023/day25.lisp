(defpackage :aoc2023.25
  (:documentation "Snowverload.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2023.25)

(defun read-graph (&optional (relative-pathname #p"2023/inputs/day25.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with graph = (make-hash-table)
            for line = (read-line in nil)
            while line
            for p = (position #\: line)
            for v = (a:make-keyword (nstring-upcase (subseq line 0 p)))
            for adjs = (mapcar
                        (lambda (s) (a:make-keyword (nstring-upcase s)))
                        (uiop:split-string (subseq line (+ 2 p))))
            do (a:appendf (gethash v graph) adjs)
               (dolist (a adjs) (push v (gethash a graph)))
            finally (return graph)))))

(defun karger-min-cut (graph
                       &aux
                         (graph (a:copy-hash-table graph))
                         (nodes (a:hash-table-keys graph))
                         (contracted (make-hash-table)))
  (flet ((pick-random-edge ()
           (let* ((u (a:random-elt nodes))
                  (v (a:random-elt (gethash u graph))))
             (values u v)))
         (contract-edge (u v)
           (dolist (w (gethash v graph))
             (a:removef (gethash w graph) v)
             (unless (eql w u)
               (push w (gethash u graph))
               (push u (gethash w graph))))
           (remhash v graph)
           (a:removef nodes v)
           (a:appendf (gethash u contracted) (list v) (gethash v contracted))
           (remhash v contracted)))
    (loop while (> (hash-table-count graph) 2)
          do (multiple-value-call #'contract-edge (pick-random-edge))
          finally (destructuring-bind (u v) nodes
                    (return (values (length (gethash u graph))
                                    (cons u (gethash u contracted))
                                    (cons v (gethash v contracted))))))))

(defun day25 (&aux (graph (read-graph)) cut c1 c2)
  (loop do (multiple-value-setq (cut c1 c2) (karger-min-cut graph))
        until (= 3 cut)
        finally (return (* (length c1) (length c2)))))

(define-test (= 518391))
