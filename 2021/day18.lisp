(defpackage :aoc2021.18
  (:documentation "Snailfish.")
  (:use :cl :aoc.utils))

(in-package :aoc2021.18)

(defun read-snailfish-numbers (&optional (relative-pathname #p"2021/inputs/day18.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil nil)
            while line
            collect (parse-snailfish-number line)))))

(defstruct (snode
            (:print-function
             (lambda (instance stream level)
               (declare (ignorable level))
               (with-slots (left right parent) instance
                 (format stream "(~A ~A)" left right)))))
  left right parent)

(defun parse-snailfish-number (string &optional (start 0))
  (if (char= (char string start) #\[)
      (let ((snum (make-snode)))
        (multiple-value-bind (left new-start)
            (parse-snailfish-number string (incf start))
          (setf start new-start
                (snode-left snum) left)
          (when (snode-p left) (setf (snode-parent left) snum)))
        (multiple-value-bind (right new-start)
            (parse-snailfish-number string (incf start))
          (setf start new-start
                (snode-right snum) right)
          (when (snode-p right) (setf (snode-parent right) snum)))
        (assert (char= (char string start) #\]))
        (values snum (1+ start)))
      (parse-integer string :start start :junk-allowed t)))

(defun deep-copy-snode (instance &aux (new (make-snode)))
  (with-slots (left right) instance
    (if (numberp left)
        (setf (snode-left new) left)
        (progn
          (setf (snode-left new) (deep-copy-snode left)
                (snode-parent (snode-left new)) new)))
    (if (numberp right)
        (setf (snode-right new) right)
        (progn
          (setf (snode-right new) (deep-copy-snode right)
                (snode-parent (snode-right new)) new))))
  new)

(defun s+ (n1 n2 &aux (new (deep-copy-snode (make-snode :left n1 :right n2))))
  (sreduce new))

(defun sreduce (snum)
  (loop for exploder = (find-snode-to-explode snum)
        for splitter = (unless exploder (find-snode-to-split snum))
        while (or exploder splitter)
        if exploder
          do (explode-snode exploder)
        else
          do (split-snode splitter)
        finally (return snum)))

(defun smagnitude (snum)
  (if (snode-p snum)
      (+ (* 3 (smagnitude (snode-left snum))) (* 2 (smagnitude (snode-right snum))))
      snum))

(defun find-snode-to-explode (snode &optional (depth 0))
  (when (snode-p snode)
    (if (>= depth 4)
        snode
        (or (find-snode-to-explode (snode-left snode) (1+ depth))
            (find-snode-to-explode (snode-right snode) (1+ depth))))))

(defun explode-snode (exploder &aux (parent (snode-parent exploder)))
  (if (eq exploder (snode-left parent))
      (progn
        (setf (snode-left parent) 0)
        (if (snode-p (snode-right parent))
            (let ((inc-lft (loop for curr = (snode-right parent) then (snode-left curr)
                                 unless (snode-p (snode-left curr)) return curr)))
              (incf (snode-left inc-lft) (snode-right exploder)))
            (incf (snode-right parent) (snode-right exploder)))
        (let ((start (loop for prev = parent then curr
                           for curr = (snode-parent parent) then (snode-parent curr)
                           while curr
                           when (eq (snode-right curr) prev) return curr)))
          (when start
            (if (snode-p (snode-left start))
                (let ((inc-rgt (loop for curr = (snode-left start) then (snode-right curr)
                                     unless (snode-p (snode-right curr)) return curr)))
                  (incf (snode-right inc-rgt) (snode-left exploder)))
                (incf (snode-left start) (snode-left exploder))))))
      (progn
        (setf (snode-right parent) 0)
        (if (snode-p (snode-left parent))
            (let ((inc-rgt (loop for curr = (snode-left parent) then (snode-right curr)
                                 unless (snode-p (snode-right curr)) return curr)))
              (incf (snode-right inc-rgt) (snode-left exploder)))
            (incf (snode-left parent) (snode-left exploder)))
        (let ((start (loop for prev = parent then curr
                           for curr = (snode-parent parent) then (snode-parent curr)
                           while curr
                           when (eq (snode-left curr) prev) return curr)))
          (when start
            (if (snode-p (snode-right start))
                (let ((inc-lft (loop for curr = (snode-right start) then (snode-left curr)
                                     unless (snode-p (snode-left curr)) return curr)))
                  (incf (snode-left inc-lft) (snode-right exploder)))
                (incf (snode-right start) (snode-right exploder))))))))

(defun find-snode-to-split (snode)
  (when (snode-p snode)
    (with-slots (left right) snode
      (if (and (numberp left) (>= left 10))
          snode
          (or (find-snode-to-split left)
              (find-snode-to-split right)
              (when (and (numberp right) (>= right 10)) snode))))))

(defun split-snode (splitter)
  (with-slots (left right) splitter
    (if (and (numberp left) (>= left 10))
        (let ((val (/ left 2)))
          (setf (snode-left splitter)
                (make-snode :left (floor val) :right (ceiling val) :parent splitter)))
        (let ((val (/ right 2)))
          (setf (snode-right splitter)
                (make-snode :left (floor val) :right (ceiling val) :parent splitter))))))

(defun day18/part-1 (snums)
  (smagnitude (reduce #'s+ snums)))

(defun day18/part-2 (snums)
  (loop for (n1 . remaining) on snums
        maximize (loop for n2 in remaining
                       maximize (smagnitude (s+ n1 n2))
                       maximize (smagnitude (s+ n2 n1)))))

(defun day18 ()
  (let ((snums (read-snailfish-numbers)))
    (values (day18/part-1 snums) (day18/part-2 snums))))

(define-test (= 4235) (= 4659))
