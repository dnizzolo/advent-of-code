(defpackage #:aoc2025.02
  (:documentation "Gift Shop.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.02)

(defun read-intervals (&optional (relative-pathname #p"2025/inputs/day02.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
        result)
    (ppcre:do-register-groups ((#'parse-integer a b))
        ("(\\d+)-(\\d+)" (uiop:read-file-string filename) result)
      (push (list a b) result))))

(defun sum-invalid (bad-fn intervals)
  (loop for (a b) in intervals
        sum (loop for i from a to b
                  when (funcall bad-fn i)
                    sum i)))

(defun bad-1 (n)
  (let* ((s (write-to-string n))
         (length (length s)))
    (and (evenp length)
         (let ((half-length (/ length 2)))
           (string= s s :end1 half-length :start2 half-length)))))

(defun bad-2 (n)
  (flet ((factors (n)
           (loop for i from 1 below n
                 when (zerop (mod n i))
                   collect i)))
    (let* ((s (write-to-string n))
           (length (length s))
           (factors (factors length)))
      (loop for fact in factors
            thereis (loop for start = fact then (+ start fact)
                          for end = (* 2 fact) then (+ end fact)
                          while (<= end length)
                          always (string= s s :end1 fact :start2 start :end2 end))))))

(defun day02 (&aux (intervals (read-intervals)))
  (values (sum-invalid #'bad-1 intervals)
          (sum-invalid #'bad-2 intervals)))

(define-test (= 15873079081) (= 22617871034))
