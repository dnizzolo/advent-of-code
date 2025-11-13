(defpackage :aoc2021.08
  (:documentation "Seven Segment Search.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.08)

(defun read-signal-patterns (&optional (relative-path #p"2021/inputs/day08.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (mapcar (lambda (part)
                              (mapcar (lambda (s) (sort s #'char<))
                                      (uiop:split-string (string-trim '(#\Space) part))))
                            (uiop:split-string line :separator '(#\|)))))))

(defun day08/part-1 (patterns)
  (loop for (signals digits) in patterns
        sum (loop for digit in digits
                  for len = (length digit)
                  count (or (= len 2)        ; 1.
                            (= len 4)        ; 4.
                            (= len 3)        ; 7.
                            (= len 7)))))    ; 8.

(defvar *seven-segments-digit*
  (a:alist-hash-table
   '(("abcefg" . 0) ("cf" . 1)
     ("acdeg" . 2) ("acdfg" . 3)
     ("bcdf" . 4) ("abdfg" . 5)
     ("abdefg" . 6) ("acf" . 7)
     ("abcdefg" . 8) ("abcdfg" . 9))
   :test #'equal))

(defun decode-signal (permutation signl)
  (let ((translated (sort (map 'string (lambda (char) (gethash char permutation)) signl)
                          #'char<)))
    (gethash translated *seven-segments-digit*)))

(defun consistent-pin-permutation-p (permutation signals)
  (loop for signl in signals
        always (decode-signal permutation signl)))

(defun find-pin-c-f (signals)
  (let ((one (find-if (lambda (signl) (= (length signl) 2)) signals))
        (zero-six-nine (remove-if-not (lambda (signl) (= (length signl) 6)) signals)))
    (loop for candidate in zero-six-nine
          do (loop for char across one
                   unless (find char candidate)
                     do (return-from find-pin-c-f
                          (values char
                                  (find-if (lambda (c) (char/= c char)) one)))))))

(defun find-pin-a (signals)
  (let ((one (find-if (lambda (signl) (= (length signl) 2)) signals))
        (seven (find-if (lambda (signl) (= (length signl) 3)) signals)))
    (loop for char across seven
          unless (find char one)
            do (return-from find-pin-a char))))

(defun make-decoder (signals &aux (permutation (make-hash-table)))
  (multiple-value-bind (c-pin f-pin) (find-pin-c-f signals)
    (let ((a-pin (find-pin-a signals)))
      (setf (gethash a-pin permutation) #\a
            (gethash c-pin permutation) #\c
            (gethash f-pin permutation) #\f)
      (let ((remaining (remove-if (lambda (c) (gethash c permutation))
                                  '(#\a #\b #\c #\d #\e #\f #\g))))
        (a:map-permutations
         (lambda (rem)
           (setf (gethash (first rem) permutation) #\b
                 (gethash (second rem) permutation) #\d
                 (gethash (third rem) permutation) #\e
                 (gethash (fourth rem) permutation) #\g)
           (if (consistent-pin-permutation-p permutation signals)
               (return-from make-decoder permutation)))
         remaining)))))

(defun day08/part-2 (patterns)
  (loop for (signals digits) in patterns
        for decoder = (make-decoder signals)
        sum (reduce (lambda (acc new) (+ new (* acc 10)))
                    digits
                    :key (lambda (dgt) (decode-signal decoder dgt)))))

(defun day08 (&aux (patterns (read-signal-patterns)))
  (values (day08/part-1 patterns) (day08/part-2 patterns)))

(define-test (= 548) (= 1074888))
