(defpackage :aoc2021.21
  (:documentation "Dirac Dice.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2021.21)

(defun read-starting-space (&optional (relative-path #p"2021/inputs/day21.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-path)))
    (values-list
     (mapcar
      (lambda (line) (second (parse-integers line)))
      (uiop:read-file-lines filename)))))

(defun play-deterministic-game (position-1 position-2)
  (flet ((take-turn (position score die rolls)
           (loop with move = 0
                 repeat 3
                 do (incf move die)
                    (setf die (1+ (mod die 100)))
                    (incf rolls)
                 finally (setf position (1+ (mod (+ position move -1) 10)))
                         (incf score position)
                         (return (values position score die rolls)))))
    (let ((score-1 0) (score-2 0) (die 1) (rolls 0))
      (loop
        (setf (values position-1 score-1 die rolls)
              (take-turn position-1 score-1 die rolls))
        (when (>= score-1 1000) (return (* score-2 rolls)))
        (setf (values position-2 score-2 die rolls)
              (take-turn position-2 score-2 die rolls))
        (when (>= score-2 1000) (return (* score-1 rolls)))))))

(defvar *frequencies*
  (a:alist-hash-table
   '((3 . 1) (4 . 3) (5 . 6) (6 . 7) (7 . 6) (8 . 3) (9 . 1))))

(define-memo-function (dirac-game (position-1 position-2 score-1 score-2))
  (cond ((>= score-1 21) (list 1 0))
        ((>= score-2 21) (list 0 1))
        (t (let ((wins-1 0) (wins-2 0))
             (maphash
              (lambda (roll count)
                (let* ((new-position-1 (1+ (mod (+ position-1 -1 roll) 10)))
                       (new-score-1 (+ score-1 new-position-1)))
                  (destructuring-bind (new-wins-1 new-wins-2)
                      (dirac-game position-2 new-position-1 score-2 new-score-1)
                    (incf wins-1 (* count new-wins-2))
                    (incf wins-2 (* count new-wins-1)))))
              *frequencies*)
             (list wins-1 wins-2)))))

(defun play-quantum-game (position-1 position-2)
  (reduce #'max (dirac-game position-1 position-2 0 0)))

(defun day21 ()
  (multiple-value-bind (position-1 position-2) (read-starting-space)
    (values (play-deterministic-game position-1 position-2)
            (play-quantum-game position-1 position-2))))

(define-test (= 604998) (= 157253621231420))
