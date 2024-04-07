(defpackage :aoc2023.19
  (:documentation "Aplenty.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2023.19)

(defun make-machine-part (x m a s)
  (list :x x :m m :a a :s s))

(defun machine-part-sum (part)
  (+ (getf part :x) (getf part :m) (getf part :a) (getf part :s)))

(defun read-machine-part (string)
  (apply #'make-machine-part (parse-integers string)))

(defstruct (condition-rule (:constructor %make-condition-rule))
  category comparator value result)

(defun make-condition-rule (category comparator value result)
  (%make-condition-rule :category category
                        :comparator comparator
                        :value value
                        :result result))

(defun read-condition-rule (string)
  (let* ((cat (ecase (char string 0) (#\x :x) (#\m :m) (#\a :a) (#\s :s)))
         (com (if (char= (char string 1) #\<) '< '>))
         (pos (position #\: string))
         (value (parse-integer string :start 2 :end pos))
         (result (a:make-keyword (nstring-upcase (subseq string (1+ pos))))))
    (make-condition-rule cat com value result)))

(defstruct (workflow (:constructor %make-workflow))
  name rules)

(defun make-workflow (name rules)
  (%make-workflow :name name :rules rules))

(defun read-workflow (string)
  (let* ((pos1 (position #\{ string)) (pos2 (position #\} string))
         (name (a:make-keyword (nstring-upcase (subseq string 0 pos1))))
         (rules (subseq string (1+ pos1) pos2)))
    (loop for rule in (uiop:split-string rules :separator '(#\,))
          collect (if (find #\: rule)
                      (read-condition-rule rule)
                      (a:make-keyword (nstring-upcase rule)))
            into rules
          finally (return (make-workflow name rules)))))

(defun read-workflows-and-parts (&optional (relative-pathname #p"2023/inputs/day19.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with workflows = (make-hash-table)
            initially (loop for line = (read-line in nil)
                            until (a:emptyp line)
                            for wf = (read-workflow line)
                            do (setf (gethash (workflow-name wf) workflows) wf))
            for line = (read-line in nil)
            while line
            collect (read-machine-part line) into parts
            finally (return (values workflows parts))))))

(defun process-machine-part (workflows part)
  (flet ((apply-workflow (workflow)
           (dolist (rule (workflow-rules (gethash workflow workflows)))
             (if (condition-rule-p rule)
                 (with-slots (category comparator value result) rule
                   (when (funcall comparator (getf part category) value)
                     (return result)))
                 (return rule)))))
    (loop with curr = :IN
          for next = (apply-workflow curr)
          do (cond ((eql next :A) (return (machine-part-sum part)))
                   ((eql next :R) (return 0))
                   (t (setf curr next))))))

(defun process-machine-parts (workflows parts)
  (loop for part in parts sum (process-machine-part workflows part)))

(defun count-ranges-combinations (ranges)
  (flet ((range-width (range) (1+ (- (cdr range) (car range)))))
    (let ((x (getf ranges :x)) (m (getf ranges :m))
          (a (getf ranges :a)) (s (getf ranges :s)))
      (* (range-width x) (range-width m) (range-width a) (range-width s)))))

(defun make-ranges ()
  (list :x (cons 1 4000) :m (cons 1 4000) :a (cons 1 4000) :s (cons 1 4000)))

(defun count-accepted-combinations (workflows &aux (total 0))
  (labels ((walk (name ranges)
             (cond ((eql name :A) (incf total (count-ranges-combinations ranges)))
                   ((eql name :R))
                   (t (dolist (rule (workflow-rules (gethash name workflows)))
                        (process-rule rule ranges)))))
           (process-rule (rule ranges)
             (if (condition-rule-p rule)
                 (with-slots (category comparator value result) rule
                   (let* ((new-ranges (copy-tree ranges))
                          (range (getf ranges category))
                          (low (car range)) (high (cdr range)))
                     (if (eql comparator '<)
                         (setf (getf new-ranges category) (cons low (1- value))
                               (getf ranges category) (cons value high))
                         (setf (getf new-ranges category) (cons (1+ value) high)
                               (getf ranges category) (cons low value)))
                     (walk result new-ranges)))
                 (walk rule ranges))))
    (walk :IN (make-ranges))
    total))

(defun day19 ()
  (multiple-value-bind (workflows parts) (read-workflows-and-parts)
    (values (process-machine-parts workflows parts)
            (count-accepted-combinations workflows))))

(define-test (= 492702) (= 138616621185978))
