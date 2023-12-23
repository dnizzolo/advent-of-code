(defpackage :aoc2023.18
  (:documentation "Lavaduct Lagoon.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.18)

(defun read-dig-plan (&optional (relative-pathname #p"2023/inputs/day18.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect (list (char line 0)
                          (parse-integer line
                                         :start (1+ (position #\Space line))
                                         :end (position #\Space line :from-end t))
                          (subseq line
                                  (1+ (position #\# line))
                                  (position #\) line :from-end t)))))))
(defun area (plan)
  (loop with area = 0
        with acc = 0
        for (dir amount) in plan
        do (case dir
             (#\R (incf area (* acc amount)))
             (#\L (decf area (* acc amount)))
             (#\U (incf acc amount))
             (#\D (decf acc amount)))
        finally (return area)))

(defun perimeter (plan)
  (reduce #'+ plan :key #'second))

(defun correct-input (plan)
  (let ((encod '((#\0 . #\R) (#\1 . #\D) (#\2 . #\L) (#\3 . #\U))))
    (loop for (d a hex) in plan
          for len-1 = (1- (length hex))
          collect (list (cdr (assoc (char hex len-1) encod))
                        (parse-integer hex :radix 16 :end len-1)))))

(defun day18 (&aux (plan (read-dig-plan)))
  (values (+ (area plan) (ash (perimeter plan) -1) 1)
          (let ((corrected (correct-input plan)))
            (+ (area corrected) (ash (perimeter corrected) -1) 1))))

(define-test (= 72821) (= 127844509405501))
