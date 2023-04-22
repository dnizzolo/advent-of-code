(defpackage :aoc.utils
  (:documentation "Utilities for Advent of Code.")
  (:use :cl)
  (:local-nicknames
   (:a :alexandria.2)
   (:bq :bodge-queue))
  (:export
   :position-2d
   :list-to-queue
   :all-different-p
   :parse-integers-from-string
   :triangular
   :bit-vector-to-integer
   :extended-gcd
   :modular-inverse
   :chinese-remainder-theorem
   :define-test))

(in-package :aoc.utils)

(defun position-2d (item array &key (test #'eql))
  "Find the first position in ARRAY which is equal to ITEM as defined by
TEST. ARRAY is a two-dimensional array. Return the two indices of the
occurrence as multiple values."
  (destructuring-bind (m n) (array-dimensions array)
    (dotimes (i m)
      (dotimes (j n)
        (when (funcall test item (aref array i j))
          (return-from position-2d (values i j)))))))

(defun list-to-queue (list &aux (q (bq:make-queue :initial-buffer-size (length list))))
  "Create a queue with the elements of LIST in the order they appear."
  (dolist (item list)
    (bq:queue-push q item))
  q)

(defun all-different-p (sequence &key (test #'eql))
  "Check if the elements in SEQUENCE are all different, i.e., SEQUENCE
is a set."
  (every (lambda (item) (= 1 (count item sequence :test test))) sequence))

(defun parse-integers-from-string (string &key (radix 10))
  "Parse all the integers in STRING ignoring other contents and return
them in a list."
  (declare (type string string))
  (loop with index = 0
        with limit = (length string)
        with parse with next-index
        while (< index limit)
        do (multiple-value-setq (parse next-index)
             (parse-integer string :start index :radix radix :junk-allowed t))
        if parse
          collect it and do (setf index next-index)
        else
          do (incf index)))

(defun triangular (n)
  "Compute the sum of the naturals from 1 to N."
  (declare (type (integer 0) n))
  (* 1/2 (* n (1+ n))))

(defun bit-vector-to-integer (bit-vector &key (start 0) (end (length bit-vector)))
  "Convert the bit-vector BIT-VECTOR from START to END (not included) to
the corresponding integer."
  (loop with result = 0
        for i from start below end
        do (setf result (+ (* 2 result) (bit bit-vector i)))
        finally (return result)))

(defun extended-gcd (a b)
  "Compute the greatest common divisor and the coefficients of Bezout's
identity for two numbers A and B using the Extended Euclidean
Algorithm."
  (declare (type integer a b))
  (loop with old-rem = a
        with rem = b
        with old-bez-s = 1
        with bez-s = 0
        with bez-t = 0
        with quotient
        with new-rem
        until (zerop rem) do
          (multiple-value-setq (quotient new-rem) (floor old-rem rem))
          (psetf old-rem rem
                 rem new-rem
                 old-bez-s bez-s
                 bez-s (- old-bez-s (* quotient bez-s)))
        finally (unless (zerop b)
                  (setf bez-t (floor (- old-rem (* a old-bez-s)) b)))
                (return (values old-rem old-bez-s bez-t))))

(defun modular-inverse (a m)
  "Compute the inverse of A modulo M."
  (declare (type integer a m))
  (multiple-value-bind (gcd a-coeff m-coeff) (extended-gcd a m)
    (declare (ignorable m-coeff))
    (if (= gcd 1)
        (mod a-coeff m)
        (error 'integer-not-invertible-in-modulo :integer a :modulo m))))

(define-condition integer-not-invertible-in-modulo (arithmetic-error)
  ((integer
    :initarg :integer
    :reader integer-not-invertible-in-modulo-integer)
   (modulo
    :initarg :modulo
    :reader integer-not-invertible-in-modulo-modulo))
  (:default-initargs
   :integer (a:required-argument :integer)
   :modulo (a:required-argument :modulo))
  (:report
   (lambda (condition stream)
     (with-slots (integer modulo) condition
       (format stream "~&~a is not invertible modulo ~a." integer modulo)))))

(defun chinese-remainder-theorem (terms moduli)
  "Apply the Chinese Remainder Theorem to solve the system of
congruences given by TERMS and MODULI and return the smallest positive
integer solution."
  (declare (type (vector integer *) terms moduli))
  (assert (= (length terms) (length moduli)))
  (let* ((n (reduce #'* moduli))
         (m (map 'vector (lambda (r) (/ n r)) moduli))
         (y (map 'vector #'modular-inverse m moduli)))
    (loop with result = 0
          for b across terms
          for k across m
          for i across y
          do (setf result (mod (+ result (mod (* b k i) n)) n))
          finally (return result))))

(defmacro define-test ((comparator-1 expected-part-1) (comparator-2 expected-part-2))
  "Define a test with expected results for both parts of the problem.

The day and hence the name of said day's function are inferred from
the name of the current package as per convention."
  (let* ((package-name (package-name *package*))
         (dotloc (position #\. package-name))
         (year-string (subseq package-name (- dotloc 4) dotloc))
         (day-string (subseq package-name (1+ dotloc)))
         (test-name (format nil "~a.~a" year-string day-string))
         (day-function-symbol (find-symbol (format nil "DAY~a" day-string))))
    `(parachute:define-test ,test-name
       (parachute:is-values (,day-function-symbol)
         (,comparator-1 ,expected-part-1)
         (,comparator-2 ,expected-part-2)))))
