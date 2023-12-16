(defpackage :aoc2022.12
  (:documentation "Hill Climbing Algorithm.")
  (:local-nicknames (:q :queue))
  (:use :cl :aoc.utils))

(in-package :aoc2022.12)

(defun read-heightmap (&optional (relative-pathname #p"2022/inputs/day12.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect line into x
            finally (return (make-array (list (length x) (length (car x)))
                                        :initial-contents x))))))

(defun accessiblep (grid start-i start-j end-i end-j)
  (flet ((elevation (char)
           (case char
             (#\S #.(char-code #\a))
             (#\E #.(char-code #\z))
             (t (char-code char)))))
    (let ((start (elevation (aref grid start-i start-j)))
          (end (elevation (aref grid end-i end-j))))
      (<= (- end start) 1))))

(defun shortest-path (grid start &key (goal #\E) descend
                      &aux
                        (q (q:make-queue))
                        (seen (make-hash-table :test #'equal)))
  (q:enqueue q start)
  (setf (gethash start seen) 0)
  (loop until (q:empty-queue-p q)
        for v = (q:dequeue q)
        for (x y) = v
        if (char= goal (aref grid x y))
          do (return-from shortest-path (gethash v seen))
        else
          do (loop for (dx dy) in '((1 0) (0 1) (-1 0) (0 -1))
                   for nx = (+ x dx) for ny = (+ y dy)
                   for u = (list nx ny)
                   when (and (array-in-bounds-p grid nx ny)
                             (not (gethash u seen))
                             (if descend
                                 (accessiblep grid nx ny x y)
                                 (accessiblep grid x y nx ny)))
                     do (q:enqueue q u)
                        (setf (gethash u seen) (1+ (gethash v seen))))))

(defun day12 (&aux (grid (read-heightmap)))
  (values
   (shortest-path grid
                  (multiple-value-list (position-2d #\S grid)))
   (shortest-path grid
                  (multiple-value-list (position-2d #\E grid))
                  :goal #\a
                  :descend t)))

(define-test (= 394) (= 388))
