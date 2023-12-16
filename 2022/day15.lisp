(defpackage :aoc2022.15
  (:documentation "Beacon Exclusion Zone.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2022.15)

(defun read-sensors (&optional (relative-pathname #p"2022/inputs/day15.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            for ints = (parse-integers-from-string line)
            collect (cons (apply #'manhattan-distance ints) ints)))))

(defun manhattan-distance (x1 y1 x2 y2) (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defun merge-intervals (intervals)
  (let ((intervals (remove-duplicates
                    (sort intervals (lambda (x y)
                                      (or (< (car x) (car y))
                                          (< (cdr x) (cdr y)))))
                    :test (lambda (x y) (= (car x) (car y))))))
    (loop with (start . end) = (first intervals)
          for (s . e) in (rest intervals)
          if (<= s end)
            do (a:maxf end e)
          else
            collect (cons start end) into result
            and do (setf start s end e)
          finally (return (nconc result (list (cons start end)))))))

(defun interval-size (interval) (1+ (- (cdr interval) (car interval))))

(defun in-interval-p (n interval) (<= (car interval) n (cdr interval)))

(defun count-impossible-beacon-positions (sensors y)
  (let* ((intervals (loop for (d sx sy) in sensors
                          for half-amplitude = (- d (abs (- y sy)))
                          unless (minusp half-amplitude)
                            collect (cons (- sx half-amplitude) (+ sx half-amplitude))))
         (merged (merge-intervals intervals)))
    (- (loop for interval in merged sum (interval-size interval))
       (loop for (bx by) in (remove-duplicates (mapcar #'cdddr sensors) :test #'equal)
             count (and (= by y) (loop for i in merged thereis (in-interval-p bx i)))))))

(defun possible-beacon-p (x y sensors)
  (loop for (d sx sy) in sensors
        always (> (manhattan-distance x y sx sy) d)))

(defun tuning-frequency (x y) (+ y (* 4000000 x)))

(defun walk-border (distance sx sy sensors)
  ;; First quadrant.
  (loop for x = (+ sx distance 1) then (1- x)
        for y = sy then (1- y)
        until (= x sx)
        when (possible-beacon-p x y sensors)
          do (return-from walk-border (tuning-frequency x y)))
  ;; Second quadrant.
  (loop for x = sx then (1- x)
        for y = (- sy distance 1) then (1+ y)
        until (= y sy)
        when (possible-beacon-p x y sensors)
          do (return-from walk-border (tuning-frequency x y)))
  ;; Third quadrant.
  (loop for x = (- sx distance 1) then (1+ x)
        for y = sy then (1+ y)
        until (= x sx)
        when (possible-beacon-p x y sensors)
          do (return-from walk-border (tuning-frequency x y)))
  ;; Fourth quadrant.
  (loop for x = sx then (1+ x)
        for y = (+ sy distance 1) then (1- y)
        until (= y sy)
        when (possible-beacon-p x y sensors)
          do (return-from walk-border (tuning-frequency x y))))

(defun day15 (&aux (sensors (read-sensors)))
  (values
   (count-impossible-beacon-positions sensors 2000000)
   (loop for (d sx sy) in sensors
         when (walk-border d sx sy sensors) return it)))

(define-test (= 5040643) (= 11016575214126))
