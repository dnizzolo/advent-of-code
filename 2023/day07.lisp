(defpackage :aoc2023.07
  (:documentation "Camel Cards.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2023.07)

(defun read-hands-and-bids (&optional (relative-pathname #p"2023/inputs/day07.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for p = (position #\Space line)
            collect (cons (subseq line 0 p) (parse-integer line :start (1+ p)))))))

(defun hand-to-string (hand &optional joker-rule-p)
  (unless joker-rule-p (setf hand (substitute #\X #\J hand)))
  (let* ((card-ranks "J23456789TXQKA")
         (type-ranks #((1 1 1 1 1) (1 1 1 2) (1 2 2) (1 1 3) (2 3) (1 4) (5)))
         (ranking (map 'list (lambda (c) (position c card-ranks)) hand)))
    (loop for c across (remove-duplicates hand)
          for sub = (substitute c #\J hand)
          for counter = (make-counter sub)
          maximize (position (sort (a:hash-table-values counter) #'<)
                             type-ranks
                             :test #'equal)
            into max
          finally (return (map 'string #'code-char (cons max ranking))))))

(defun total-winnings (hands-bids)
  (loop for i from 1 for (h . b) in hands-bids sum (* i b)))

(defun day07 (&aux (hands-bids (read-hands-and-bids)))
  (values (total-winnings (sort (copy-seq hands-bids)
                                #'string<
                                :key (lambda (x) (hand-to-string (car x)))))
          (total-winnings (sort hands-bids
                                #'string<
                                :key (lambda (x) (hand-to-string (car x) t))))))

(define-test (= 248836197) (= 251195607))
