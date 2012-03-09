;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defun $finite-p (x)
  (df-finite-p x))

(defun $infinity-p (x)
  (df-infinity-p x))

(defun $nan-p (x)
  (df-not-a-number-p x))

(defun $snan-p (x)
  (and (df-not-a-number-p x)
       (df-negative-p x)))

(defun $qnan-p (x)
  (and (df-not-a-number-p x)
       (not (df-negative-p x))))

(defun $subnormal-p (x)
  (df-subnormal-p x))

(defun $normal-p (x)
  (and (df-normal-p x)
       (not (df-zerop x))))

(defun $zerop (x)
  (and (df-normal-p x)
       (df-zero-or-subnormal-p x)))

(defun $minusp (x)
  (and ($signed-p x)
       (or (not (eql 0 (df-slots x)))
	   ($infinity-p x))))

(defun $signed-p (x)
  (and (df-negative-p x)
       (not (df-not-a-number-p x))'))

(defun $plusp (x)
  (not (or (df-negative-p x)
           (df-not-a-number-p x)
           (df-zerop x))))

(defsetf $logb #'(lambda (a b) ($scaleb b a)))

(defun $canonical-p (x)
  (with-inf-nan-handler (x :+infinity (eq x ++infinity+)
			   :-infinity (eq x +-infinity+)
			   :qnan (eq x +qnan+)
			   :snan (eq x +snan+))
    (zerop (df-carry-bit x))))

(defun $logb (x)
  (with-inf-nan-handler (x :infinity ++infinity+
			   :qnan +qnan+ :snan +snan+)
    (%logb x)))

(defun $count-digits (x)
  (with-inf-nan-handler (x :infinity ++infinity+
			   :nan x)
    (%count-digits x)))

(defun $shift (x digits &optional (copy-p t))
  (if ($finite-p x)
      (multiple-value-bind (dslots digits) (floor digits +decimal-slot-digits+)
	(let* ((y (if copy-p (copy-df x) x))
	       (old-slots (length (df-slots y)))
	       (old-lsd (df-last-slot-digits y))
	       (new-lsd (- old-lsd digits))
	       (slots (- old-slots
			 dslots
			 (if (minusp new-lsd)
			     (progn
			       (incf new-lsd +decimal-slot-digits+)
			       1)
			     0))))
	  (setf (df-slots y)
		(adjust-array (if (and +simple-array-adjustable-p+ copy-p)
				  (copy-array (df-slots y))
				  (df-slots y))
			      slots
			      :initial-element 0)
		(df-last-slot-digits y) new-lsd)
	  y))
      x))
