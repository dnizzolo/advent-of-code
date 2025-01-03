(defpackage :aoc2024.10
  (:documentation "Hoof It.")
  (:local-nicknames (:q :queue))
  (:use :cl :aoc.utils))

(in-package :aoc2024.10)

(defun read-map (&optional (relative-pathname #p"2024/inputs/day10.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (map 'list #'digit-char-p line) into lines
            finally (return (make-array (list (length lines) (length (first lines)))
                                        :initial-contents lines))))))

(defun visit-1 (map sr sc)
  (loop with acc = 0
        with seen = (make-hash-table :test #'equal)
        with queue = (q:make-queue)
        initially (q:enqueue queue (list sr sc))
                  (setf (gethash (list sr sc) seen) t)
        until (q:empty-p queue)
        do (destructuring-bind (r c) (q:dequeue queue)
             (let ((height (aref map r c)))
               (when (= 9 height) (incf acc))
               (loop for (dr dc) in '((-1 0) (0 1) (0 -1) (1 0))
                     for nr = (+ r dr) for nc = (+ c dc)
                     when (and (array-in-bounds-p map nr nc)
                               (= (1+ height) (aref map nr nc))
                               (not (gethash (list nr nc) seen)))
                       do (q:enqueue queue (list nr nc))
                          (setf (gethash (list nr nc) seen) t))))
        finally (return acc)))

(defun visit-2 (map sr sc)
  (loop with acc = 0
        with queue = (q:make-queue)
        initially (q:enqueue queue (list sr sc))
        until (q:empty-p queue)
        do (destructuring-bind (r c) (q:dequeue queue)
             (let ((height (aref map r c)))
               (when (= 9 height) (incf acc))
               (loop for (dr dc) in '((-1 0) (0 1) (0 -1) (1 0))
                     for nr = (+ r dr) for nc = (+ c dc)
                     when (and (array-in-bounds-p map nr nc)
                               (= (1+ height) (aref map nr nc)))
                       do (q:enqueue queue (list nr nc)))))
        finally (return acc)))

(defun day10 (&aux (map (read-map)))
  (destructuring-bind (rows columns) (array-dimensions map)
    (let ((part-1 0) (part-2 0))
      (dotimes (i rows)
        (dotimes (j columns)
          (when (zerop (aref map i j))
            (incf part-1 (visit-1 map i j))
            (incf part-2 (visit-2 map i j)))))
      (values part-1 part-2))))

(define-test (= 825) (= 1805))
