(defpackage :aoc2022.10
  (:documentation "Cathode-Ray Tube.")
  (:local-nicknames (:a :alexandria.2))
  (:use :cl :aoc.utils))

(in-package :aoc2022.10)

(defclass instruction ()
  ((cycles :reader cycles)))

(defclass noop (instruction) ())

(defmethod initialize-instance :after ((inst noop) &key)
  (setf (slot-value inst 'cycles) 1))

(defclass addx (instruction)
  ((value :initarg :value :reader addx-value))
  (:default-initargs
   :value (a:required-argument :value)))

(defmethod initialize-instance :after ((inst addx) &key)
  (setf (slot-value inst 'cycles) 2))

(defclass machine-state ()
  ((x :accessor x-register :initform 1)
   (clock :accessor clock :initform 0)
   (samples :accessor samples :initform nil)
   (screen :accessor screen :initform (make-array '(6 40)))))

(defmethod tick ((state machine-state))
  (with-slots (x clock samples screen) state
    (let ((columns (array-dimension screen 1)))
      (setf (row-major-aref screen clock)
            (if (<= (abs (- x (mod clock columns))) 1) #\Full_Block #\.)))
    (incf clock)
    (when (sample-clock-p clock)
      (push (list clock x) samples))))

(defun sample-clock-p (clock)
  (zerop (mod (- clock 20) 40)))

(defmethod run :around ((inst instruction) (state machine-state))
  (dotimes (_ (cycles inst)) (tick state))
  (call-next-method))

(defmethod run ((inst noop) (state machine-state)))

(defmethod run ((inst addx) (state machine-state))
  (incf (x-register state) (addx-value inst)))

(defun read-program (&optional (relative-pathname #p"2022/inputs/day10.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (with-open-file (in filename)
      (loop for line = (read-line in nil) while line
            for (p1 p2) = (uiop:split-string line)
            collect (if (string= p1 "noop")
                        (make-instance 'noop)
                        (make-instance 'addx :value (parse-integer p2)))))))

(defun day10 (&aux (program (read-program)) (state (make-instance 'machine-state)))
  (dolist (inst program) (run inst state))
  (values
   (loop for (cyc x) in (samples state) sum (* cyc x))
   (destructuring-bind (rows columns) (array-dimensions (screen state))
     (with-output-to-string (s)
       (let ((out (make-broadcast-stream s *standard-output*)))
         (terpri out)
         (dotimes (i rows)
           (dotimes (j columns)
             (princ (aref (screen state) i j) out))
           (terpri out)))))))

(define-test
    (= 11820)
    (string= "
████.███....██.███..███..█..█..██..█..█.
█....█..█....█.█..█.█..█.█.█..█..█.█..█.
███..█..█....█.███..█..█.██...█..█.████.
█....███.....█.█..█.███..█.█..████.█..█.
█....█....█..█.█..█.█.█..█.█..█..█.█..█.
████.█.....██..███..█..█.█..█.█..█.█..█.
"))
