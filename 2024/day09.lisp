(defpackage :aoc2024.09
  (:documentation "Disk Fragmenter.")
  (:use :cl :aoc.utils))

(in-package :aoc2024.09)

(defun read-disk-map (&optional (relative-pathname #p"2024/inputs/day09.txt"))
  (let ((filename (asdf:system-relative-pathname :advent-of-code relative-pathname)))
    (uiop:read-file-line filename)))

(defun expand-blocks-1 (disk-map)
  (loop with blocks = (make-array 8 :adjustable t :fill-pointer 0)
        for i from 0
        for char across disk-map
        for amount = (digit-char-p char)
        for item = (if (evenp i) (ash i -1) nil)
        do (dotimes (j amount) (vector-push-extend item blocks))
        finally (return blocks)))

(defun move-blocks-1 (blocks)
  (loop with length = (length blocks)
        with i = 0
        with j = (1- length)
        while (< i j)
        do (cond ((aref blocks i) (incf i))
                 ((null (aref blocks j)) (decf j))
                 (t (rotatef (aref blocks i) (aref blocks j))
                    (incf i)
                    (decf j)))
        finally (return blocks)))

(defun expand-blocks-2 (disk-map)
  (loop with files = (make-hash-table)
        with blanks = (make-array 8 :adjustable t :fill-pointer 0)
        with file-id = 0
        with position = 0
        for i from 0
        for char across disk-map
        for amount = (digit-char-p char)
        do (if (zerop (mod i 2))
               (setf (gethash file-id files) (list position amount)
                     file-id (1+ file-id))
               (unless (zerop amount)
                 (vector-push-extend (list position amount) blanks)))
           (incf position amount)
        finally (return (values files blanks file-id))))

(defun day09 (&aux (disk-map (read-disk-map)))
  (values (loop with blocks = (move-blocks-1 (expand-blocks-1 disk-map))
                for i from 0
                for block across blocks
                while block
                sum (* i block))
          (multiple-value-bind (files blanks max-file-id)
              (expand-blocks-2 disk-map)
            (loop while (plusp max-file-id)
                  do (decf max-file-id)
                     (destructuring-bind (position size) (gethash max-file-id files)
                       (loop for i from 0
                             for blank across blanks
                             for (start length) = blank
                             do (when (>= start position)
                                  (setf blanks (subseq blanks 0 i))
                                  (loop-finish))
                                (when (<= size length)
                                  (setf (gethash max-file-id files) (list start size))
                                  (if (= size length)
                                      (setf blanks (delete blank blanks :start i :count 1))
                                      (setf (aref blanks i) (list (+ start size) (- length size))))
                                  (loop-finish))))
                  finally (return
                            (let ((total 0))
                              (maphash
                               (lambda (file value)
                                 (loop with position = (first value)
                                       with size = (second value)
                                       for i from position below (+ position size)
                                       do (incf total (* file i))))
                               files)
                              total))))))

(define-test (= 6384282079460) (= 6408966547049))
