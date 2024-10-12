(defpackage :aoc2023.20
  (:documentation "Pulse Propagation.")
  (:local-nicknames (:a :alexandria.2) (:q :queue))
  (:use :cl :aoc.utils))

(in-package :aoc2023.20)

(defun read-module-configuration (&optional (relative-pathname #p"2023/inputs/day20.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop with result = nil
            for line = (read-line in nil)
            while line
            do (destructuring-bind (module outs)
                   (ppcre:split " -> " line)
                 (let ((outputs (coerce (ppcre:split ", " outs) 'vector))
                       class name)
                   (cond ((string= module "broadcaster")
                          (setf class 'broadcaster
                                name "broadcaster"))
                         ((char= (char module 0) #\%)
                          (setf class 'flip-flop
                                name (subseq module 1)))
                         ((char= (char module 0) #\&)
                          (setf class 'conjunction
                                name (subseq module 1)))
                         (t (error "Module of unknown kind ~a." module)))
                   (push (make-instance class :name name :outputs outputs)
                         result)))
            finally (return (backpatch result))))))

(defun backpatch (modules &aux untypeds)
  (flet ((find-module (name)
           (or (find name modules :key #'module-name :test #'string=)
               (cdr (assoc name untypeds :test #'string=))
               (progn
                 (let ((new (make-instance 'untyped :name name :outputs #())))
                   (push new untypeds)
                   new)))))
    (dolist (module modules modules)
      (setf (module-outputs module)
            (map 'vector #'find-module (module-outputs module)))
      (loop for output across (module-outputs module)
            when (conjunction-p output)
              do (vector-push-extend :low (conjunction-lasts output))
                 (vector-push-extend module (conjunction-inputs output))))))

(defclass module ()
  ((name :initarg :name :accessor module-name)
   (outputs :initarg :outputs :accessor module-outputs))
  (:default-initargs
   :name (a:required-argument :name)
   :outputs (a:required-argument :outputs)))

(defun module-p (object)
  (typep object 'module))

(defclass flip-flop (module)
  ((state :initform :off :accessor flip-flop-state)))

(defun flip-flop-p (object)
  (typep object 'flip-flop))

(defclass conjunction (module)
  ((lasts :initform (make-array 4 :adjustable t :fill-pointer 0) :accessor conjunction-lasts)
   (inputs :initform (make-array 4 :adjustable t :fill-pointer 0) :accessor conjunction-inputs)
   (sent-high-p :initform nil :accessor conjunction-sent-high-p)))

(defun conjunction-p (object)
  (typep object 'conjunction))

(defclass broadcaster (module) ())

(defun broadcaster-p (object)
  (typep object 'broadcaster))

(defclass untyped (module) ())

(defun untyped-p (object)
  (typep object 'untyped))

(defgeneric reset-to-initial-state (module))

(defmethod reset-to-initial-state ((module flip-flop))
  (setf (slot-value module 'state) :off))

(defmethod reset-to-initial-state ((module conjunction))
  (fill (slot-value module 'lasts) :low)
  (setf (slot-value module 'sent-high-p) nil))

(defmethod reset-to-initial-state ((module broadcaster)))

(defmethod reset-to-initial-state ((module untyped)))

(defgeneric process-pulse (pulse sender receiver))

(defmethod process-pulse ((pulse (eql :high)) sender (module flip-flop)))

(defmethod process-pulse ((pulse (eql :low)) sender (module flip-flop))
  (with-slots (state) module
    (ecase state
      (:off
       (setf state :on)
       (schedule-output-pulse :high module))
      (:on
       (setf state :off)
       (schedule-output-pulse :low module)))))

(defmethod process-pulse (pulse sender (module conjunction))
  (with-slots (lasts inputs) module
    (setf (aref lasts (position sender inputs)) pulse)
    (let ((pulse (if (every (lambda (pin) (eql pin :high)) lasts) :low :high)))
      (schedule-output-pulse pulse module))))

(defmethod process-pulse ((pulse (eql :low)) sender (module broadcaster))
  (schedule-output-pulse :low module))

(defmethod process-pulse (pulse sender (module untyped)))

(defgeneric schedule-output-pulse (pulse sender))

(defmethod schedule-output-pulse (pulse (sender module))
  (loop for receiver across (module-outputs sender)
        do (schedule-pulse pulse sender receiver)))

(defmethod schedule-output-pulse :after (pulse (sender conjunction))
  (when (eql pulse :high)
    (setf (conjunction-sent-high-p sender) t)))

(defvar *the-agenda*)

(defvar *low-pulse-count* 0)
(defvar *high-pulse-count* 0)

(defun schedule-pulse (pulse sender receiver)
  (ecase pulse
    (:low (incf *low-pulse-count*))
    (:high (incf *high-pulse-count*)))
  (q:enqueue *the-agenda* (list pulse sender receiver)))

(defun simulate ()
  (do ()
      ((q:empty-p *the-agenda*))
    (destructuring-bind (pulse sender receiver)
        (q:dequeue *the-agenda*)
      (process-pulse pulse sender receiver))))

(defun push-button (modules)
  (let ((*the-agenda* (q:make-queue)))
    (schedule-pulse :low :button (find-if #'broadcaster-p modules))
    (simulate)))

(defun push-button-1000-times (modules)
  (let ((*low-pulse-count* 0)
        (*high-pulse-count* 0))
    (dotimes (i 1000)
      (push-button modules))
    (* *low-pulse-count* *high-pulse-count*)))

(defun reset-modules-to-initial-state (modules)
  (dolist (module modules)
    (reset-to-initial-state module)))

(defun count-pushes-to-high-pulse (module modules)
  (loop initially (reset-modules-to-initial-state modules)
        for n from 1
        do (push-button modules)
           (when (conjunction-sent-high-p module)
             (return n))))

(defun day20 (&aux (modules (read-module-configuration)))
  (values (push-button-1000-times modules)
          (let* ((input-to-rx (find-if
                               (lambda (module)
                                 (find-if #'untyped-p (module-outputs module)))
                               modules))
                 (modules-to-watch (conjunction-inputs input-to-rx)))
            (reduce #'lcm
                    (loop for module across modules-to-watch
                          collect (count-pushes-to-high-pulse module modules))))))

(define-test (= 896998430) (= 236095992539963))
