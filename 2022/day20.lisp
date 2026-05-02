(defpackage #:aoc2022.20
  (:documentation "Grove Positioning System.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2022.20)

(defun read-encrypted (&optional (relative-pathname #p"2022/inputs/day20.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (mapcar #'parse-integer (uiop:read-file-lines filename))))

(defstruct node value prev next)

(defmethod print-object ((object node) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (node-value object))))

(defun build-ring (numbers &optional (key 1))
  (loop with length = (length numbers)
        with ring = (map 'vector (lambda (n) (make-node :value  (* n key))) numbers)
        for i below length
        do (setf (node-prev (aref ring i)) (aref ring (mod (1- i) length))
                 (node-next (aref ring i)) (aref ring (mod (1+ i) length)))
        finally (return ring)))

(defun mix (nodes)
  (loop with length = (length nodes)
        for node across nodes
        for value = (node-value node)
        for steps = (mod value (1- length))
        unless (zerop steps)
          do (setf (node-next (node-prev node)) (node-next node)
                   (node-prev (node-next node)) (node-prev node))
             (let* ((target (loop with curr = node
                                  repeat steps
                                  do (setf curr (node-next curr))
                                  finally (return curr)))
                    (after (node-next target)))
               (setf (node-next target) node
                     (node-prev node) target
                     (node-next node) after
                     (node-prev after) node))
        finally (return nodes)))

(defun grove-sum (nodes)
  (loop with zero = (find 0 nodes :key #'node-value)
        with curr = zero
        for i from 1 to 3000
        do (setf curr (node-next curr))
        when (or (= i 1000) (= i 2000) (= i 3000))
          sum (node-value curr)))

(defun day20 (&aux (numbers (read-encrypted)))
  (values (grove-sum (mix (build-ring numbers)))
          (grove-sum (loop with nodes = (build-ring numbers 811589153)
                           repeat 10
                           do (setf nodes (mix nodes))
                           finally (return nodes)))))

(define-test (= 1591) (= 14579387544492))
