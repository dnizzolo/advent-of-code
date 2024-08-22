(defpackage :aoc2021.19
  (:documentation "Beacon Scanner.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.19)

(defun read-scanners (&optional (relative-path #p"2021/inputs/day19.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop for id from 0
            for points = (loop initially (read-line in nil)
                               for line = (read-line in nil)
                               while (plusp (length line))
                               collect (coerce (parse-integers line) 'vector)
                                 into inter-result
                               finally (return (coerce inter-result 'vector)))
            while (plusp (length points))
            collect points into result
            finally (return (coerce result 'vector))))))

(defun v- (u v)
  (vector (- (aref u 0) (aref v 0))
          (- (aref u 1) (aref v 1))
          (- (aref u 2) (aref v 2))))

(defun v+ (u v)
  (vector (+ (aref u 0) (aref v 0))
          (+ (aref u 1) (aref v 1))
          (+ (aref u 2) (aref v 2))))

(defun compute-3-cube-rotations (&aux result)
  (dolist (p '(#(0 1 2) #(1 2 0) #(2 0 1)))
    (dolist (s '(#(1 1 1) #(1 -1 -1) #(-1 1 -1) #(-1 -1 1)))
      (push (cons p s) result)))
  (dolist (p '(#(0 2 1) #(2 1 0) #(1 0 2)))
    (dolist (s '(#(-1 -1 -1) #(-1 1 1) #(1 -1 1) #(1 1 -1)))
      (push (cons p s) result)))
  result)

(defparameter *3-cube-rotations* (compute-3-cube-rotations))

(defun rotate (rotation v)
  (destructuring-bind (perm . sign) rotation
    (vector (* (aref v (aref perm 0)) (aref sign 0))
            (* (aref v (aref perm 1)) (aref sign 1))
            (* (aref v (aref perm 2)) (aref sign 2)))))

(defun align-two (reference other &aux (deltas (make-hash-table :test #'equalp)))
  (dolist (rotation *3-cube-rotations*)
    (clrhash deltas)
    (loop for r1 across reference
          do (loop for r2 across other
                   do (incf (gethash (v- r1 (rotate rotation r2)) deltas 0))))
    (destructuring-bind (delta . count)
        (reduce (lambda (acc new) (if (> (cdr new) (cdr acc)) new acc))
                (a:hash-table-alist deltas))
      (when (>= count 12)
        (return (values rotation delta))))))

(defun align-all (scanners)
  (let* ((len (length scanners))
         (aligned (make-array len :fill-pointer 0))
         (done (make-hash-table))
         (offsets (make-array len :fill-pointer 0)))
    (vector-push (aref scanners 0) aligned)
    (vector-push #(0 0 0) offsets)
    (setf (gethash 0 done) t)
    (loop until (= (length aligned) len)
          do (loop for i from 1 below len
                   do (unless (gethash i done)
                        (loop for scanner across aligned
                              for j from 0
                              do (multiple-value-bind (rot delta)
                                     (align-two scanner (aref scanners i))
                                   (when rot
                                     (vector-push
                                      (map 'vector
                                           (lambda (x) (rotate rot x))
                                           (aref scanners i))
                                      aligned)
                                     (setf (gethash i done) t)
                                     (vector-push (v+ delta (aref offsets j)) offsets)
                                     (return)))))))
    (values aligned offsets)))

(defun count-beacons (aligned offsets &aux (bag (make-hash-table :test #'equalp)))
  (loop with n = (length aligned)
        for i below n
        for offset = (aref offsets i)
        do (loop for c across (aref aligned i)
                 do (incf (gethash (v+ c offset) bag 0))))
  (hash-table-count bag))

(defun manhattan-distance (u v)
  (reduce #'+ (map 'vector (lambda (a b) (abs (- a b))) u v)))

(defun max-distance (positions)
  (loop with n = (length positions)
        for i below n
        for u = (aref positions i)
        maximize (loop for j from (1+ i) below n
                       for v = (aref positions j)
                       maximize (manhattan-distance u v))))

(defun day19 (&aux (scanners (read-scanners)))
  (multiple-value-bind (aligned offsets) (align-all scanners)
    (values (count-beacons aligned offsets)
            (max-distance offsets))))

(define-test (= 432) (= 14414))
