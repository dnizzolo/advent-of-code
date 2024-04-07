(defpackage :aoc2022.11
  (:documentation "Monkey in the Middle.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.11)

(defstruct monkey
  items
  operation
  divisor
  target-true
  target-false)

(defun read-monkeys (&optional (relative-pathname #p"2022/inputs/day11.txt"))
  (flet ((parse-initial-items (string)
           (let ((ints (parse-integers string)))
             (make-array (length ints)
                         :initial-contents ints
                         :adjustable t
                         :fill-pointer t)))
         (parse-operation (string)
           (let ((int (car (parse-integers string)))
                 (mulp (find #\* string)))
             (if int
                 (if mulp
                     (lambda (old) (* int old))
                     (lambda (old) (+ int old)))
                 (if mulp
                     (lambda (old) (* old old))
                     (lambda (old) (+ old old))))))
         (parse-number (string)
           (car (parse-integers string))))
    (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
      (with-open-file (in filename)
        (loop with monkey
              for line = (read-line in nil)
              while line
              when (plusp (length line))
                do (let ((items (parse-initial-items (read-line in)))
                         (operation (parse-operation (read-line in)))
                         (divisor (parse-number (read-line in)))
                         (target-true (parse-number (read-line in)))
                         (target-false (parse-number (read-line in))))
                     (setf monkey (make-monkey :items items
                                               :operation operation
                                               :divisor divisor
                                               :target-true target-true
                                               :target-false target-false)))
                and collect monkey into monkeys
              finally (return (make-array (length monkeys)
                                          :initial-contents monkeys)))))))

(defun run-monkeys (monkeys &key (steps 20) worried)
  (let ((inspects (make-array (length monkeys) :initial-element 0))
        (lcm (apply #'lcm (map 'list #'monkey-divisor monkeys))))
    (dotimes (_ steps)
      (loop for mon across monkeys for i from 0 do
        (with-slots (items operation divisor target-true target-false) mon
          (unless (zerop (length items))
            (incf (svref inspects i) (length items))
            (loop for item across items
                  for worry = (if worried
                                  (mod (funcall operation item) lcm)
                                  (floor (/ (funcall operation item) 3)))
                  for divisible = (zerop (mod worry divisor))
                  do (vector-push-extend
                      worry
                      (monkey-items
                       (svref monkeys (if divisible target-true target-false)))))
            (setf (fill-pointer items) 0)))))
    (setf inspects (sort inspects #'>))
    (* (svref inspects 0) (svref inspects 1))))

(defun day11 ()
  (values (run-monkeys (read-monkeys))
          (run-monkeys (read-monkeys) :steps 10000 :worried t)))

(define-test (= 58786) (= 14952185856))
