(defpackage :aoc2021.02
  (:documentation "Dive!")
  (:use :cl :aoc.utils))

(in-package :aoc2021.02)

(defun read-submarine-commands ()
  (mapcar (lambda (line)
            (let ((spl (uiop:split-string line)))
              (list (intern (string-upcase (car spl)) :keyword)
                    (parse-integer (cadr spl)))))
          (uiop:read-file-lines
           (asdf:system-relative-pathname :advent-of-code "2021/inputs/day02.txt"))))

(defun day02/part-1 (commands &aux (horiz 0) (depth 0))
  (dolist (comm commands (* horiz depth))
    (let ((dirc (car comm))
          (step (cadr comm)))
      (ecase dirc
        (:forward (incf horiz step))
        (:up (decf depth step))
        (:down (incf depth step))))))

(defun day02/part-2 (commands &aux (horiz 0) (depth 0) (aim 0))
  (dolist (comm commands (* horiz depth))
    (let ((dirc (car comm))
          (step (cadr comm)))
      (ecase dirc
        (:forward (incf horiz step) (incf depth (* aim step)))
        (:up (decf aim step))
        (:down (incf aim step))))))

(defun day02 ()
  (let ((commands (read-submarine-commands)))
    (values (day02/part-1 commands) (day02/part-2 commands))))

(define-test (= 2150351) (= 1842742223))
