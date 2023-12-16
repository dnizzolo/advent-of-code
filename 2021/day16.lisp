(defpackage :aoc2021.16
  (:documentation "Packet Decoder.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.16)

(defun read-packet (&optional (relative-pathname #p"2021/inputs/day16.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (parse-packet (hex-string-to-bit-vector (uiop:read-file-line filename)))))

(defun hex-string-to-bit-vector (string)
  (loop with bitv = (make-array 4096 :element-type 'bit :fill-pointer 0 :adjustable t)
        for hex-digit across string
        for int = (digit-char-p hex-digit 16)
        do (loop for i from 3 downto 0
                 do (vector-push-extend (if (logbitp i int) 1 0) bitv))
        finally (return bitv)))

(defclass packet ()
  ((version
    :initarg :version
    :reader version)
   (type-id
    :initarg :type-id
    :reader type-id)))

(defclass literal-packet (packet)
  ((literal
    :initarg :literal
    :reader literal)))

(defclass operator-packet (packet)
  ((length-type-id
    :initarg :length-type-id
    :reader length-type-id)
   (sub-packets
    :initarg :sub-packets
    :reader sub-packets)))

(defun parse-packet (bv &optional (start 0))
  (let ((version (bit-vector-to-integer bv :start start :end (incf start 3)))
        (type-id (bit-vector-to-integer bv :start start :end (incf start 3))))
    (if (= type-id 4)
        (multiple-value-bind (literal start)
            (parse-literal-packet bv start)
          (values (make-instance
                   'literal-packet
                   :version version
                   :type-id type-id
                   :literal literal)
                  start))
        (multiple-value-bind (length-type-id sub-packets start)
            (parse-operator-packet bv start)
          (values (make-instance
                   'operator-packet
                   :version version
                   :type-id type-id
                   :length-type-id length-type-id
                   :sub-packets sub-packets)
                  start)))))

(defun parse-literal-packet (bv &optional (start 0) &aux (literal 0))
  (loop (let ((morep (= (bit bv start) 1)))
          (setf literal (+ (* literal 16)
                           (bit-vector-to-integer bv
                                                  :start (incf start)
                                                  :end (incf start 4))))
          (unless morep (return (values literal start))))))

(defun parse-operator-packet (bv &optional (start 0))
  (let ((length-type-id (prog1 (bit bv start) (incf start))))
    (if (zerop length-type-id)
        (loop with bits = (bit-vector-to-integer bv :start start :end (incf start 15))
              until (= consumed bits)
              for (new-packet new-start) = (multiple-value-list (parse-packet bv start))
              sum (- new-start start) into consumed
              collect new-packet into sub-packets
              do (setf start new-start)
              finally (return (values length-type-id sub-packets start)))
        (loop with count = (bit-vector-to-integer bv :start start :end (incf start 11))
              repeat count
              for (new-packet new-start) = (multiple-value-list (parse-packet bv start))
              collect new-packet into sub-packets
              do (setf start new-start)
              finally (return (values length-type-id sub-packets start))))))

(defgeneric treeduce (function obj &key key))

(defmethod treeduce (function (obj literal-packet) &key (key #'identity))
  (declare (ignorable function))
  (funcall key obj))

(defmethod treeduce (function (obj operator-packet) &key (key #'identity))
  (reduce function
          (sub-packets obj)
          :key (lambda (sub) (treeduce function sub :key key))
          :initial-value (funcall key obj)))

(defgeneric evaluate (obj))

(defmethod evaluate ((obj literal-packet))
  (literal obj))

(defmethod evaluate ((obj operator-packet))
  (let ((operations #(#'+ #'* #'min #'max nil
                      (lambda (p q) (if (> p q) 1 0))
                      (lambda (p q) (if (< p q) 1 0))
                      (lambda (p q) (if (= p q) 1 0)))))
    (reduce (eval (aref operations (type-id obj))) (sub-packets obj) :key #'evaluate)))

(defun day16 ()
  (let ((packet (read-packet)))
    (values
     (treeduce #'+ packet :key #'version)
     (evaluate packet))))

(define-test (= 945) (= 10637009915279))
