(defpackage :aoc2022.06
  (:documentation "Tuning Trouble.")
  (:use :cl :aoc.utils))

(in-package :aoc2022.06)

(defun read-datastream-buffer (&optional (relative-pathname #p"2022/inputs/day06.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (uiop:read-file-string filename)))

(defun datastream-start-marker (data window-size)
  (let ((set 0))
    (dotimes (i (length data))
      (setf set (logxor set
                        (ash 1 (- (char-code (char data i))
                                  #.(char-code #\a)))))
      (when (>= i window-size)
        (setf set (logxor set
                          (ash 1 (- (char-code (char data (- i window-size)))
                                    #.(char-code #\a))))))
      (when (= window-size (logcount set))
        (return (1+ i))))))

(defun day06 ()
  (let ((data (read-datastream-buffer)))
    (values (datastream-start-marker data 4)
            (datastream-start-marker data 14))))

(define-test (= 1343) (= 2193))
