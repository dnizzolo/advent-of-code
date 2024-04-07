(defpackage :aoc2023.12
  (:documentation "Hot Springs.")
  (:local-nicknames (:s :serapeum))
  (:use :cl :aoc.utils))

(in-package :aoc2023.12)

(defun read-records (&optional (relative-pathname #p"2023/inputs/day12.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for p = (position #\Space line)
            collect (cons (subseq line 0 p)
                          (coerce (parse-integers line :start (1+ p)) 'vector))))))

(defun count-arrangements (configuration groups
                           &aux
                             (n (length configuration))
                             (m (length groups)))
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0) (compilation-speed 0)))
  (declare (type simple-string configuration))
  (declare (type (simple-array fixnum (*)) groups))
  (declare (type fixnum n m))
  (let ((dp (make-array (list (1+ n) (1+ m)) :element-type 'fixnum)))
    (setf (aref dp n m) 1)
    (loop for j below m do (setf (aref dp n j) 0))
    (loop with p = (or (position #\# configuration :from-end t) -1)
          for i from (1+ p) below n do (setf (aref dp i m) 1))
    (loop for i from (1- n) downto 0 do
      (loop for j from (1- m) downto 0 do
        (let ((result 0)
              (char (char configuration i))
              (group (aref groups j))
              (remaining (- n i)))
          (declare (type fixnum result))
          (when (member char '(#\. #\?))
            (incf result (aref dp (1+ i) j)))
          (when (and (member char '(#\# #\?))
                     (<= group remaining)
                     (loop for k from i below (+ i group)
                           never (char= #\. (char configuration k)))
                     (or (= group remaining)
                         (char/= #\# (char configuration (+ i group)))))
            (incf result (aref dp (min n (+ i group 1)) (1+ j))))
          (setf (aref dp i j) result))))
    (aref dp 0 0)))

(defun expand-record (record)
  (cons (s:string-join (make-list 5 :initial-element (car record)) #\?)
        (s:repeat-sequence (cdr record) 5)))

(defun day12 (&aux (records (read-records)))
  (values (loop for (conf . groups) in records
                sum (count-arrangements conf groups))
          (loop for record in records
                for (conf . groups) = (expand-record record)
                sum (count-arrangements conf groups))))

(define-test (= 7490) (= 65607131946466))
