(defpackage :aoc2021.04
  (:documentation "Giant Squid.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.04)

(defclass bingo-board ()
  ((numbers :initarg :board-numbers :accessor bingo-board-numbers)
   (marked-numbers :accessor bingo-board-marked))
  (:default-initargs
   :board-numbers (a:required-argument :board-numbers)))

(defmethod initialize-instance :after ((instance bingo-board) &key)
  (setf (bingo-board-marked instance) (make-array '(5 5) :initial-element nil)))

(defun make-bingo-board (numbers)
  (make-instance 'bingo-board
                 :board-numbers
                 (make-array '(5 5) :initial-contents numbers)))

(defmethod print-object ((board bingo-board) stream)
  (with-slots (numbers marked-numbers) board
    (dotimes (i 5)
      (fresh-line stream)
      (dotimes (j 5)
        (format stream
                "~3,' d~a"
                (aref numbers i j)
                (if (aref marked-numbers i j) #\# #\Space))))))

(defmethod mark ((object bingo-board) number)
  (with-slots (numbers marked-numbers) object
    (dotimes (i 5)
      (dotimes (j 5)
        (setf (aref marked-numbers i j)
              (or (aref marked-numbers i j)
                  (= number (aref numbers i j))))))))

(defmethod winnerp ((object bingo-board))
  (with-slots (numbers marked-numbers) object
    (let ((win (loop for i below 5
                     thereis (or (loop for j below 5 always (aref marked-numbers i j))
                                 (loop for j below 5 always (aref marked-numbers j i))))))
      (when win
        (loop for i below 5
              sum (loop for j below 5
                        unless (aref marked-numbers i j)
                          sum (aref numbers i j)))))))

(defun mark-number (number boards) (loop for board in boards do (mark board number)))

(defun read-bingo-game (&optional (rel-path #p"2021/inputs/day04.txt")
                        &aux (filename (asdf:system-relative-pathname :advent-of-code rel-path)))
  (with-open-file (in filename)
    (values (parse-integers (prog1 (read-line in nil) (read-line in nil)))
            (loop while (listen in)
                  collect (make-bingo-board (loop for line = (read-line in nil)
                                                  while (plusp (length line))
                                                  collect (parse-integers line)))))))

(defun day04/part-1 (extractions boards)
  (loop named outer for number in extractions
        do (loop with winp = nil
                 for board in boards
                 do (mark board number)
                    (setf winp (winnerp board))
                 when (winnerp board)
                   do (return-from outer (* number winp)))))

(defun day04/part-2 (extractions boards &aux last-winner)
  (let ((last-number (loop for previous = nil then number
                           for number in extractions
                           until (every #'winnerp boards)
                           do (loop with winp = nil
                                    for board in boards
                                    do (mark board number)
                                       (setf winp (winnerp board))
                                    when winp
                                      do (setf last-winner winp
                                               boards (delete board boards)))
                           finally (return previous))))
    (* last-number last-winner)))

(defun day04 ()
  (multiple-value-bind (extractions boards) (read-bingo-game)
    (values (day04/part-1 extractions boards)
            (day04/part-2 extractions boards))))

(define-test (= 12796) (= 18063))
