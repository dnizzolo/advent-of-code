(defpackage :aoc2022.07
  (:documentation "No Space Left On Device.")
  (:local-nicknames
   (:a :alexandria.2)
   (:omrn :one-more-re-nightmare))
  (:use :cl :aoc.utils))

(in-package :aoc2022.07)

(defclass entry ()
  ((name :initarg :name :accessor name))
  (:default-initargs
   :name (a:required-argument :name)))

(defclass file (entry)
  ((size :initarg :size :accessor file-size)
   (parent :initarg :parent :accessor parent))
  (:default-initargs
   :size (a:required-argument :size)
   :parent (a:required-argument :parent)))

(defclass dir (entry)
  ((entries :initarg :entries :accessor dir-entries)
   (parent :initarg :parent :accessor dir-parent))
  (:default-initargs
   :entries nil
   :parent nil))

(defmethod size ((object file))
  (file-size object))

(defmethod size ((object dir))
  (reduce #'+ (dir-entries object) :key #'size))

(defmethod subdirs ((object dir))
  (remove-if-not (lambda (x) (typep x 'dir)) (dir-entries object)))

(defmethod find-dir ((object dir) dir-name)
  (loop for sub in (subdirs object)
        until (string= (name sub) dir-name)
        finally (return sub)))

(defun read-command-output (&optional (relative-pathname #p"2022/inputs/day07.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname))
        (regex (omrn:compile-regular-expression "($ )*«[¬ ]+» *«[¬ ]*»")))
    (flet ((match (string)
             (let ((result (omrn:first-string-match regex string)))
               (list (svref result 1) (svref result 2)))))
      (with-open-file (in filename)
        (loop with root-dir = (make-instance 'dir :name "/")
              with current-dir = root-dir
              for line = (read-line in nil)
              while line
              for (p1 p2) = (match line)
              do (cond ((string= p1 "cd")
                        (setf current-dir
                              (cond ((string= p2 "/") root-dir)
                                    ((string= p2 "..") (dir-parent current-dir))
                                    (t (find-dir current-dir p2)))))
                       ((string= p1 "dir")
                        (push (make-instance 'dir :name p2 :parent current-dir)
                              (dir-entries current-dir)))
                       ((every #'digit-char-p p1)
                        (push (make-instance 'file
                                             :name p2
                                             :parent current-dir
                                             :size (parse-integer p1))
                              (dir-entries current-dir))))
              finally (return root-dir))))))

(defun sum-size-of-dirs (root &aux (max-size 100000))
  (loop for e in (subdirs root)
        if (<= (size e) max-size) collect e into within
          else collect e into oversized
        finally (return
                  (+ (reduce #'+ within :key #'size)
                     (loop for e in within sum (sum-size-of-dirs e))
                     (loop for e in oversized sum (sum-size-of-dirs e))))))

(defconstant +total-space+ 70000000)
(defconstant +required-space+ 30000000)

(defun smallest-size-to-delete (root needed &aux (min (size root)))
  (loop for e in (subdirs root)
        when (>= (size e) needed)
          do (a:minf min (smallest-size-to-delete e needed))
        finally (return min)))

(defun day07 ()
  (let ((fs (read-command-output)))
    (values (sum-size-of-dirs fs)
            (smallest-size-to-delete fs (+ +required-space+ (size fs) (- +total-space+))))))

(define-test (= 2031851) (= 2568781))
