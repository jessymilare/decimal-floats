;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2013 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defun integer-to-decimal (integer)
  (let ((abs-integer (abs integer)))
    (cond
      ;; Assuming INTEGER is probably a fixnum
      ((< abs-integer +maximum-decimal-slot+)
       (make-decimal-float 0 (make-digits-array 1 :initial-element abs-integer)
                           :negative-p (minusp integer)
                           :last-slot-first-digit (first-digit-of-slot integer)))
      ((< abs-integer (expt +maximum-decimal-slot+ 2))
       (multiple-value-bind (s1 s0) (truncate abs-integer +maximum-decimal-slot+)
         (let ((slots (make-digits-array 2)))
           (setf (aref slots 0) s0
                 (aref slots 1) s1)
           (make-decimal-float 1 slots :negative-p (minusp integer)
                               :last-slot-first-digit (first-digit-of-slot s1)))))
      ;; INTEGER is large...
      (t
       (let* ((length
               (loop for length = (ceiling (log abs-integer +maximum-decimal-slot+))
                  then (1+ length)
                  for top = (expt +maximum-decimal-slot+ length)
                  then (* top +maximum-decimal-slot+)
                  until (< abs-integer top)
                  finally (return length)))
              (slots (make-digits-array length)))
         (multiple-value-bind (%integer slot)
             (truncate abs-integer +maximum-decimal-slot+)
           (dotimes (i (1- length))
             (setf (aref slots i) slot
                   (values %integer slot)
                   (truncate %integer +maximum-decimal-slot+)))
           ;; Last slot
           (assert (zerop %integer))
           (setf (aref slots (1- length)) slot)
           (make-decimal-float (1- length) slots :negative-p (minusp integer)
                               :last-slot-first-digit (first-digit-of-slot slot))))))))

(defun decimal-to-integer (x)
  (with-operation (decimal-to-integer condition x)
      ()
    (with-inf-nan-handler (x :any (decimal-error-cond (x)
                                    (decimal-invalid-operation)))
      (let* ((slots (df-slots x))
             (length (length slots))
             (iexponent (df-iexponent x))
             (fsld (df-first-slot-last-digit x))
             (exponent (%df-exponent iexponent length fsld)))
        (unless (zerop exponent)
          (decimal-error-cond ('to-be-calculated)
            (decimal-invalid-operation))
          (if (minusp iexponent)
              (return-from decimal-to-integer 0)))
        (let ((start (- length 1 iexponent))
              (first-pow 1))
          (if (< start 0)
              (setf first-pow (expt +maximum-decimal-slot+ (- start))
                    start 0))
          (loop for i from start below length
             for pow = first-pow then (* pow +maximum-decimal-slot+)
             summing (* pow (aref slots i))))))))
