(defpackage :aoc.utils
  (:documentation "Utilities for Advent of Code.")
  (:use :cl)
  (:local-nicknames
   (:a :alexandria.2)
   (:q :queue))
  (:export
   #:count-digits
   #:detect-cycle
   #:power-of-2-p
   #:with-memoization
   #:define-memo-function
   #:clear-memo
   #:make-counter
   #:position-2d
   #:2d-array->list
   #:list->queue
   #:all-different-p
   #:all-same-p
   #:parse-integers
   #:triangular
   #:bit-vector->integer
   #:extended-gcd
   #:modular-inverse
   #:chinese-remainder-theorem
   #:define-test))

(in-package :aoc.utils)

(defun count-digits (number &optional (base 10))
  "Return the number of digits needed to represent NUMBER in base BASE."
  (declare (type (integer 0) number))
  (1+ (floor (log number base))))

(defun detect-cycle (function initial-value &key (test #'eql))
  "Find a cycle in the sequence of iterated FUNCTION values, starting at
INITIAL-VALUE using Brent's algorithm. Equality is checked by the
function TEST. Return as multiple values the length of the cycle, the
index at which the cycle starts and the first value in the cycle."
  (let (power lambda mu tortoise hare start-value)
    (loop initially (setf power 1
                          lambda 1
                          tortoise initial-value
                          hare (funcall function initial-value))
          until (funcall test tortoise hare)
          when (= power lambda)
            do (setf tortoise hare
                     power (ash power 1)
                     lambda 0)
          do (setf hare (funcall function hare))
             (incf lambda))
    (loop initially (setf tortoise initial-value
                          hare initial-value)
          repeat lambda
          do (setf hare (funcall function hare)))
    (loop initially (setf mu 0)
          until (funcall test tortoise hare)
          do (setf tortoise (funcall function tortoise)
                   hare (funcall function hare))
             (incf mu)
          finally (setf start-value tortoise))
    (values lambda mu start-value)))

(defun power-of-2-p (n)
  "Check if N is equal to 2 to the power of some natural number."
  (declare (type (integer 0) n))
  (= 1 (logcount n)))

(defmacro with-memoization ((hash-table &rest key-forms) &body body)
  "Memoize the results of BODY into HASH-TABLE under the key obtained by
 evaluating all the KEY-FORMS. HASH-TABLE is expected to be a symbol
 naming an hash-table. If BODY exits by means of a non-local transfer
 of control, the associated return value is not cached in HASH-TABLE."
  (declare (type symbol hash-table))
  (a:with-gensyms (table key value foundp)
    `(let ((,table ,hash-table)
           (,key (list ,@key-forms)))
       (multiple-value-bind (,value ,foundp)
           (gethash ,key ,table)
         (if ,foundp
             ,value
             (setf (gethash ,key ,table)
                   (progn ,@body)))))))

(defmacro define-memo-function ((name lambda-list &rest key-forms) &body body)
  "Define a memoized function at top level. If KEY-FORMS are provided
they are used to create a list that serves as key into the hash-table.
Forms in KEY-FORMS may refer to parameters in the LAMBDA-LIST."
  `(memoize-defun (defun ,name ,lambda-list ,@body)
                  ,(when key-forms
                     `(lambda ,lambda-list (list ,@key-forms)))))

(defun memoize-defun (name key-fn)
  "Replace NAME's global function definition with its memoized
 version. Store the hash table for memoization in symbol NAME's plist
 under the key :MEMO."
  (flet ((memo (function)
           (let ((table (make-hash-table :test #'equal))
                 (key-fn (or key-fn #'identity)))
             (setf (get name :memo) table)
             (lambda (&rest args)
               (multiple-value-bind (value foundp)
                   (gethash (apply key-fn args) table)
                 (if foundp
                     value
                     (setf (gethash (apply key-fn args) table)
                           (apply function args))))))))
    (setf (fdefinition name) (memo (fdefinition name)))))

(defun clear-memo (name)
  "Clear the hash table of NAME's memoized function."
  (let ((table (get name :memo)))
    (when table (clrhash table))))

(defgeneric make-counter (bag &key key test)
  (:documentation "Return an hash-table that maps the items in BAG to their counts."))

(defmethod make-counter ((bag list) &key key (test #'eql))
  (loop with counter = (make-hash-table :test test)
        with key-fn = (or key #'identity)
        for item in bag
        do (incf (gethash (funcall key-fn item) counter 0))
        finally (return counter)))

(defmethod make-counter ((bag vector) &key key (test #'eql))
  (loop with counter = (make-hash-table :test test)
        with key-fn = (or key #'identity)
        for item across bag
        do (incf (gethash (funcall key-fn item) counter 0))
        finally (return counter)))

(defun position-2d (item array &key (test #'eql))
  "Find the first position in ARRAY which is equal to ITEM as defined by
TEST. ARRAY is a two-dimensional array. Return the two indices of the
occurrence as multiple values."
  (declare (type (array * (* *)) array))
  (destructuring-bind (m n) (array-dimensions array)
    (dotimes (i m)
      (dotimes (j n)
        (when (funcall test item (aref array i j))
          (return-from position-2d (values i j)))))))

(defun 2d-array->list (array)
  "Convert a two-dimensional array into a list of lists by rows."
  (declare (type (array * (* *)) array))
  (destructuring-bind (m n) (array-dimensions array)
    (loop for i below m
          collect (loop for j below n
                        collect (aref array i j)))))

(defun list->queue (list &aux (q (q:make-queue (length list))))
  "Create a queue with the elements of LIST in the order they appear."
  (dolist (item list q) (q:enqueue q item)))

(defun all-different-p (sequence &key (test #'eql))
  "Check if the elements in SEQUENCE are all different, i.e., SEQUENCE
is a set."
  (every (lambda (item) (= 1 (count item sequence :test test))) sequence))

(defun all-same-p (sequence &key (test #'eql) &aux (len (length sequence)))
  "Check if the elements in SEQUENCE are all the same."
  (or (zerop len) (= len (count (elt sequence 0) sequence :test test))))

(defun parse-integers (string &key (start 0) end (radix 10))
  "Parse all the integers in STRING ignoring other contents and return
them in a list."
  (declare (type string string))
  (loop with start = start
        with end = (or end (length string))
        with parse with next-start
        while (< start end)
        do (multiple-value-setq (parse next-start)
             (parse-integer string :start start :end end :radix radix :junk-allowed t))
        if parse
          collect it and do (setf start next-start)
        else
          do (incf start)))

(defun triangular (n)
  "Compute the sum of the naturals from 1 to N."
  (declare (type (integer 0) n))
  (* 1/2 (* n (1+ n))))

(defun bit-vector->integer (bit-vector &key (start 0) (end (length bit-vector)))
  "Convert the bit-vector BIT-VECTOR from START to END (not included) to
the corresponding integer."
  (declare (type bit-vector bit-vector))
  (loop with result = 0
        for i from start below end
        do (setf result (+ (* 2 result) (bit bit-vector i)))
        finally (return result)))

(defun modular-expt (base power modulus)
  "Return BASE raised to the POWER modulo MODULUS."
  (declare (type integer base power modulus))
  (cond ((= modulus 1) 0)
        ((minusp power)
         (modular-expt (modular-inverse base modulus) (- power) modulus))
        (t
         (loop with result = 1
               with base = (mod base modulus)
               while (plusp power)
               when (logand power 1)
                 do (setf result (mod (* result base) modulus))
               do (setf power (ash power -1)
                        base (mod (* base base) modulus))
               finally (return result)))))

(defun extended-gcd (a b)
  "Compute the greatest common divisor and the coefficients of Bezout's
identity for two numbers A and B using the Extended Euclidean
Algorithm."
  (declare (type integer a b))
  (loop with old-rem = a
        with rem = (abs b)
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

(defun modular-inverse (number modulus)
  "Compute the inverse of NUMBER modulo MODULUS."
  (declare (type integer number modulus))
  (multiple-value-bind (gcd a-coeff m-coeff) (extended-gcd number modulus)
    (declare (ignorable m-coeff))
    (if (= gcd 1)
        (mod a-coeff modulus)
        (error 'number-not-invertible-in-modulo
               :number number
               :modulus modulus))))

(define-condition number-not-invertible-in-modulo (arithmetic-error)
  ((number
    :initarg :number
    :reader number-not-invertible-in-modulo-number)
   (modulus
    :initarg :modulus
    :reader number-not-invertible-in-modulo-modulus))
  (:default-initargs
   :number (a:required-argument :number)
   :modulus (a:required-argument :modulus))
  (:report
   (lambda (condition stream)
     (with-slots (number modulus) condition
       (format stream "~a is not invertible modulo ~a." number modulus)))))

(defun chinese-remainder-theorem (terms moduli)
  "Apply the Chinese Remainder Theorem to solve the system of
congruences given by TERMS and MODULI and return the smallest positive
integer solution."
  (declare (type (vector integer) terms moduli))
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

(defmacro define-test (&body comparators)
  "Define a test with expected results for parts of the problem.
The day and hence the name of said day's function are inferred from
the name of the current package as per convention."
  (let* ((package-name (package-name *package*))
         (dotloc (position #\. package-name))
         (year-string (subseq package-name (- dotloc 4) dotloc))
         (day-string (subseq package-name (1+ dotloc)))
         (test-name (format nil "~a.~a" year-string day-string))
         (day-function-symbol (find-symbol (format nil "DAY~a" day-string))))
    `(parachute:define-test ,test-name
       ,(if (rest comparators)
            `(parachute:is-values (,day-function-symbol)
               ,@comparators)
            ;; Extract COMP and EXPECT from ((COMP EXPECT)).
            `(,(caar comparators) ,(cadar comparators) (,day-function-symbol))))))
