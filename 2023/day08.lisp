(defpackage :aoc2023.08
  (:documentation "Haunted Wastelands.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2023.08)

(defun read-network (&optional (relative-pathname #p"2023/inputs/day08.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with network = (make-hash-table :test #'equal)
            with instructions = (prog1 (read-line in nil) (read-line in nil))
            for line = (read-line in nil)
            while line
            do (setf (gethash (subseq line 0 3) network)
                     (cons (subseq line 7 10) (subseq line 12 15)))
            finally (return (values instructions network))))))

(defun follow-instructions (instructions network start end-predicate)
  (loop with curr = start
        with len = (length instructions)
        for i = 0 then (mod (1+ i) len)
        for step from 0
        until (funcall end-predicate curr)
        for (left . right) = (gethash curr network)
        for inst = (char instructions i)
        for next = (if (char= inst #\L) left right)
        do (setf curr next)
        finally (return step)))

(defun day08 ()
  (multiple-value-bind (instructions network)
      (read-network)
    (values (follow-instructions
             instructions network "AAA" (lambda (c) (string= c "ZZZ")))
            (loop for item in (remove-if-not (lambda (n) (char= #\A (char n 2)))
                                             (a:hash-table-keys network))
                  collect (follow-instructions
                           instructions network item (lambda (c) (char= #\Z (char c 2))))
                    into steps
                  finally (return (apply #'lcm steps))))))

(define-test (= 16343) (= 15299095336639))
