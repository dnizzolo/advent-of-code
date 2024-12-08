(defpackage :aoc2024.08
  (:documentation "Resonant Collinearity.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2024.08)

(defun read-map (&optional (relative-pathname #p"2024/inputs/day08.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil)
            while line
            collect line into lines
            finally (return (make-array (list (length lines) (length (first lines)))
                                        :initial-contents lines))))))

(defun antenna-positions (map &aux (positions (make-hash-table)))
  (destructuring-bind (rows cols) (array-dimensions map)
    (dotimes (i rows positions)
      (dotimes (j cols)
        (let ((item (aref map i j)))
          (unless (char= #\. item)
            (push (list i j) (gethash item positions))))))))

(defun distance (p q)
  (+ (abs (- (first p) (first q))) (abs (- (second p) (second q)))))

(defun collinear-p (a b c)
  (= (* (- (second a) (second b)) (- (first a) (first c)))
     (* (- (second a) (second c)) (- (first a) (first b)))))

(defun day08 ()
  (let* ((map (read-map))
         (antenna-positions (antenna-positions map))
         (seen-1 (make-hash-table :test #'equal))
         (seen-2 (make-hash-table :test #'equal)))
    (destructuring-bind (rows cols) (array-dimensions map)
      (dotimes (i rows)
        (dotimes (j cols)
          (let ((p (list i j)))
            (maphash
             (lambda (antenna positions)
               (declare (ignore antenna))
               (a:map-combinations
                (lambda (pair)
                  (destructuring-bind (p1 p2) pair
                    (when (collinear-p p p1 p2)
                      (setf (gethash p seen-2) t)
                      (let ((dist1 (distance p p1))
                            (dist2 (distance p p2)))
                        (when (or (= dist1 (* 2 dist2)) (= dist2 (* 2 dist1)))
                          (setf (gethash p seen-1) t))))))
                positions
                :length 2))
             antenna-positions)))))
    (values (hash-table-count seen-1) (hash-table-count seen-2))))

(define-test (= 222) (= 884))
