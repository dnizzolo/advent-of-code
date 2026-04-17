(defpackage #:aoc2025.05
  (:documentation "Cafeteria.")
  (:local-nicknames (#:s #:serapeum))
  (:use #:cl #:aoc.utils))

(in-package #:aoc2025.05)

(defun read-data (&optional (relative-pathname #p"2025/inputs/day05.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (let (ranges ids)
        (loop for line = (read-line in nil)
              while (plusp (length line))
              do (push (parse-all-integers
                        (substitute #\Space #\- line))
                       ranges))
        (loop for line = (read-line in nil)
              while line
              do (push (parse-integer line) ids))
        (values ranges ids)))))

(defun in-range-p (n range)
  (<= (first range) n (second range)))

(defun merge-ranges (range1 range2)
  (list (min (first range1) (first range2))
        (max (second range1) (second range2))))

(defun merge-all-ranges (ranges)
  (labels ((range-overlap-or-adjacent-p (range1 range2)
             (<= (max (first range1) (first range2))
                 (1+ (min (second range1) (second range2)))))
           (merge-range-into (range ranges)
             (multiple-value-bind (overlapping non-overlapping)
                 (s:partition (lambda (r) (range-overlap-or-adjacent-p r range))
                              ranges)
               (if overlapping
                   (let ((merged (reduce (lambda (acc r) (merge-ranges acc r))
                                         overlapping
                                         :initial-value range)))
                     (merge-range-into merged non-overlapping))
                   (adjoin range ranges :test #'equal)))))
    (reduce (lambda (acc r) (merge-range-into r acc))
            ranges
            :initial-value nil)))

(defun day05 ()
  (multiple-value-bind (ranges ids) (read-data)
    (let ((ranges (merge-all-ranges ranges)))
      (values (loop for id in ids
                    count (loop for range in ranges thereis (in-range-p id range)))
              (loop for (a b) in ranges sum (1+ (- b a)))))))

(define-test (= 726) (= 354226555270043))
