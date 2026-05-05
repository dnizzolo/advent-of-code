(defpackage #:aoc2022.21
  (:documentation "Monkey Math.")
  (:use #:cl #:aoc.utils))

(in-package #:aoc2022.21)

(defun read-monkey-jobs (&optional (relative-pathname #p"2022/inputs/day21.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for name = (subseq line 0 4)
            if (digit-char-p (char line 6))
              collect (list name (parse-integer line :start 6))
            else
              collect (list name
                            (subseq line 6 10)
                            (ecase (char line 11)
                              (#\+ '+) (#\- '-) (#\* '*) (#\/ '/))
                            (subseq line 13))))))

(defun eval-monkeys (monkeys)
  (multiple-value-bind (cache non-resolved)
      (loop with cache = (make-hash-table :test #'equal)
            for monkey in monkeys
            if (integerp (second monkey))
              do (setf (gethash (first monkey) cache) (second monkey))
            else
              collect monkey into non-primitive
            finally (return (values cache non-primitive)))
    (loop while non-resolved
          do (loop for monkey in non-resolved
                   for (name left op right) = monkey
                   for left-operand = (gethash left cache)
                   for right-operand = (gethash right cache)
                   if (and left-operand right-operand)
                     do (setf (gethash name cache)
                              (funcall op left-operand right-operand))
                   else
                     collect monkey into backlog
                   finally (setf non-resolved backlog))
          finally (return cache))))

(defun find-humn-value (monkeys monkey-values)
  (labels ((invert (op)
             (ecase op (+ '-) (- '+) (* '/) (/ '*)))
           (visit-to-humn (node value)
             (if (string= node "humn")
                 value
                 (destructuring-bind (name left &optional op right)
                     (find node monkeys :key #'first :test #'string=)
                   (declare (ignorable name))
                   (if (integerp left)
                       nil
                       (let ((inverse (invert op)))
                         (ecase op
                           ((+ *)
                            (or (visit-to-humn
                                 left (funcall inverse value (gethash right monkey-values)))
                                (visit-to-humn
                                 right (funcall inverse value (gethash left monkey-values)))))
                           ((- /)
                            (or (visit-to-humn
                                 left (funcall inverse value (gethash right monkey-values)))
                                (visit-to-humn
                                 right (funcall op (gethash left monkey-values) value)))))))))))
    (destructuring-bind (name left equal right)
        (find "root" monkeys :key #'first :test #'string=)
      (declare (ignorable name equal))
      (or (visit-to-humn left (gethash right monkey-values))
          (visit-to-humn right (gethash left monkey-values))))))

(defun day21 ()
  (let* ((monkeys (read-monkey-jobs))
         (monkey-values (eval-monkeys monkeys)))
    (values (gethash "root" monkey-values)
            (find-humn-value monkeys monkey-values))))

(define-test (= 41857219607906) (= 3916936880448))
