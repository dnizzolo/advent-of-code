(defpackage :aoc2023.05
  (:documentation "If You Give A Seed A Fertilizer.")
  (:use :cl :aoc.utils))

(in-package :aoc2023.05)

(defun read-almanac (&optional (relative-pathname #p"2023/inputs/day05.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with seeds = (parse-integers (prog1 (read-line in nil)
                                           (read-line in nil)))
            while (listen in)
            collect (loop initially (read-line in nil)
                          for line = (read-line in nil)
                          while (plusp (length line)) collect (parse-integers line))
              into maps
            finally (return (values seeds maps))))))

(defun thread-value-through-maps (value maps)
  (flet ((apply-map-to-value (map value)
           (loop for (destination source delta) in map
                 for diff = (- value source)
                 when (<= 0 diff delta)
                   return (+ destination diff)
                 finally (return value))))
    (loop for map in maps
          do (setf value (apply-map-to-value map value))
          finally (return value))))

(defun match-range (range fixed)
  (let ((s1 (first range)) (l1 (second range))
        (s2 (first fixed)) (l2 (second fixed)))
    (cond ((and (<= s1 s2) (<= l1 (- s2 s1)))
           (values nil range nil))
          ((<= s1 s2)
           (let* ((matched (list s2 (min l2 (- l1 (- s2 s1)))))
                  (delta-before (- s2 s1))
                  (before (list s1 delta-before))
                  (delta-after (- (+ s1 l1) (+ s2 l2)))
                  (after (list (+ s2 l2) delta-after)))
             (values matched
                     (and (plusp delta-before) before)
                     (and (plusp delta-after) after))))
          ((and (<= s2 s1) (<= l2 (- s1 s2)))
           (values nil nil range))
          ((<= s2 s1)
           (let* ((matched (list s1 (min l1 (- l2 (- s1 s2)))))
                  (delta-after (- (+ s1 l1) (+ s2 l2)))
                  (after (list (+ s2 l2) delta-after)))
             (values matched nil (and (plusp delta-after) after)))))))

(defun apply-map-to-range (map range)
  (loop with result with remaining = (list range)
        for (destination source delta) in map
        do (loop with next-remaining with matched with before with after
                 for range in remaining
                 do (multiple-value-setq (matched before after)
                      (match-range range (list source delta)))
                    (when before (push before next-remaining))
                    (when after (push after next-remaining))
                    (when matched
                      (let ((diff (- (first matched) source)))
                        (push (list (+ destination diff) (second matched)) result)))
                 finally (setf remaining next-remaining))
        finally (return (append result remaining))))

(defun thread-range-through-maps (range maps)
  (loop with ranges = (list range)
        for map in maps
        do (setf ranges (loop for range in ranges append (apply-map-to-range map range)))
        finally (return ranges)))

(defun day05 ()
  (multiple-value-bind (seeds maps)
      (read-almanac)
    (values (loop for seed in seeds minimize (thread-value-through-maps seed maps))
            (loop for (x y) on seeds by #'cddr
                  for range = (list x y)
                  minimize (reduce #'min
                                   (thread-range-through-maps range maps)
                                   :key #'first)))))

(define-test (= 323142486) (= 79874951))
