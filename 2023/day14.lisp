(defpackage :aoc2023.14
  (:documentation "Parabolic Reflector Dish.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.14)

(defun read-platform (&optional (relative-pathname #p"2023/inputs/day14.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect line into rows
            finally (return (make-array (list (length rows) (length (first rows)))
                                        :initial-contents rows
                                        :element-type 'character))))))

(defun load-on-north-beams (platform &aux (result 0))
  (destructuring-bind (n m) (array-dimensions platform)
    (dotimes (i n result)
      (dotimes (j m)
        (when (char= #\O (aref platform i j)) (incf result (- n i)))))))

(defun move-north (platform)
  (destructuring-bind (n m) (array-dimensions platform)
    (let ((new (make-array (list n m) :initial-element #\. :element-type 'character)))
      (dotimes (i n new)
        (dotimes (j m)
          (let ((char (aref platform i j)))
            (cond ((char= #\O char)
                   (loop with os = 0
                         for k from (1- i) downto 0
                         for c = (aref platform k j)
                         until (char= #\# c)
                         when (char= #\O c)
                           do (incf os)
                         finally (setf (aref new (+ (1+ k) os) j) #\O)))
                  ((char= #\# char)
                   (setf (aref new i j) #\#)))))))))

(defun move-south (platform)
  (destructuring-bind (n m) (array-dimensions platform)
    (let ((new (make-array (list n m) :initial-element #\. :element-type 'character)))
      (dotimes (i n new)
        (dotimes (j m)
          (let ((char (aref platform i j)))
            (cond ((char= #\O char)
                   (loop with os = 0
                         for k from (1+ i) below n
                         for c = (aref platform k j)
                         until (char= #\# c)
                         when (char= #\O c)
                           do (incf os)
                         finally (setf (aref new (- (1- k) os) j) #\O)))
                  ((char= #\# char)
                   (setf (aref new i j) #\#)))))))))

(defun move-east (platform)
  (destructuring-bind (n m) (array-dimensions platform)
    (let ((new (make-array (list n m) :initial-element #\. :element-type 'character)))
      (dotimes (i n new)
        (dotimes (j m)
          (let ((char (aref platform i j)))
            (cond ((char= #\O char)
                   (loop with os = 0
                         for k from (1+ j) below m
                         for c = (aref platform i k)
                         until (char= #\# c)
                         when (char= #\O c)
                           do (incf os)
                         finally (setf (aref new i (- (1- k) os)) #\O)))
                  ((char= #\# char)
                   (setf (aref new i j) #\#)))))))))

(defun move-west (platform)
  (destructuring-bind (n m) (array-dimensions platform)
    (let ((new (make-array (list n m) :initial-element #\. :element-type 'character)))
      (dotimes (i n new)
        (dotimes (j m)
          (let ((char (aref platform i j)))
            (cond ((char= #\O char)
                   (loop with os = 0
                         for k from (1- j) downto 0
                         for c = (aref platform i k)
                         until (char= #\# c)
                         when (char= #\O c)
                           do (incf os)
                         finally (setf (aref new i (+ (1+ k) os)) #\O)))
                  ((char= #\# char)
                   (setf (aref new i j) #\#)))))))))

(defun platform->string (platform)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (declare (type (simple-array character (* *)) platform))
  (destructuring-bind (n m) (array-dimensions platform)
    (declare (type (integer 0 1024) n m))
    (let ((result (make-string (* n m))))
      (loop for i below n do
        (loop for j below m do
          (setf (char result (+ j (* i m))) (aref platform i j))))
      result)))

(define-memo-function (cycle (platform) (platform->string platform))
  (move-east (move-south (move-west (move-north platform)))))

(defun platform= (platform1 platform2)
  (destructuring-bind (n m) (array-dimensions platform1)
    (loop for i below n
          always (loop for j below m
                       always (char= (aref platform1 i j) (aref platform2 i j))))))

(defun day14 (&aux (platform (read-platform)))
  (values (load-on-north-beams (move-north platform))
          (multiple-value-bind (cycle-size cycle-start start-value)
              (detect-cycle #'cycle platform :test #'platform=)
            (let ((remaining (mod (- 1000000000 cycle-start) cycle-size)))
              (loop for p = start-value then (cycle p)
                    repeat remaining
                    finally (return (load-on-north-beams p)))))))

(define-test (= 103614) (= 83790))
