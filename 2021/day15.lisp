(defpackage :aoc2021.15
  (:documentation "Chiton.")
  (:local-nicknames (:dfpq :damn-fast-priority-queue))
  (:use :cl :aoc.utils))

(in-package :aoc2021.15)

(defun read-risk-levels (&optional (relative-path #p"2021/inputs/day15.txt"))
  (let* ((filename (asdf:system-relative-pathname :advent-of-code relative-path))
         (lvls (with-open-file (in filename)
                 (loop for line = (read-line in nil)
                       while line
                       collect (map 'list #'digit-char-p line)))))
    (make-array (list (length lvls) (length (first lvls))) :initial-contents lvls)))

;; A modified Dijkstra's algorithm. The key insight is that all the
;; edges that end in a vertex have the same weight therefore
;; Dijkstra's algorithm would never relax a vertex by discovering a
;; better edge to get to the vertex after considering the neighbour of
;; said vertex which is closest to the source.
(defun risk-search (source goal &key neighbours risk
                    &aux
                      (enqueued (make-hash-table :test #'equal))
                      (frontier (dfpq:make-queue)))
  (setf (gethash source enqueued) 0)
  (dfpq:enqueue frontier source 0)
  (loop while (dfpq:peek frontier)
        for vertex = (dfpq:dequeue frontier)
        for cost = (gethash vertex enqueued)
        if (equal vertex goal)
          return cost
        do (loop for next in (loop for neigh in (funcall neighbours vertex)
                                   unless (gethash neigh enqueued) collect neigh)
                 for next-cost = (+ cost (funcall risk next))
                 do (setf (gethash next enqueued) next-cost)
                    (dfpq:enqueue frontier next next-cost))))

(defun day15/part-1 (risk-levels rows cols)
  (risk-search '(0 0)
               (list (1- rows) (1- cols))
               :neighbours (lambda (v)
                             (loop for (di dj) in '((0 1) (0 -1) (1 0) (-1 0))
                                   for ni = (+ (first v) di)
                                   for nj = (+ (second v) dj)
                                   for n = (list ni nj)
                                   when (and (< -1 ni rows) (< -1 nj cols))
                                     collect n))
               :risk (lambda (v)
                       (apply #'aref risk-levels v))))

(defun day15/part-2 (risk-levels rows cols &aux (ext-rows (* 5 rows)) (ext-cols (* 5 cols)))
  (risk-search '(0 0)
               (list (1- ext-rows) (1- ext-cols))
               :neighbours (lambda (v)
                             (loop for (di dj) in '((0 1) (0 -1) (1 0) (-1 0))
                                   for ni = (+ (first v) di)
                                   for nj = (+ (second v) dj)
                                   for n = (list ni nj)
                                   when (and (< -1 ni ext-rows) (< -1 nj ext-cols))
                                     collect n))
               :risk (lambda (v)
                       (multiple-value-bind (q1 r1) (floor (first v) rows)
                         (multiple-value-bind (q2 r2) (floor (second v) cols)
                           (let ((risk (+ q1 q2 (aref risk-levels r1 r2))))
                             (if (< risk 10) risk (- risk 9))))))))

(defun day15 ()
  (let ((risk-levels (read-risk-levels)))
    (destructuring-bind (rows cols) (array-dimensions risk-levels)
      (values (day15/part-1 risk-levels rows cols)
              (day15/part-2 risk-levels rows cols)))))

(define-test (= 487) (= 2821))
