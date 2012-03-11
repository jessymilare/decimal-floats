;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (inline finite-p))
(defun finite-p (x)
  (df-finite-p x))

(declaim (inline infinity-p))
(defun infinite-p (x)
  (df-infinity-p x))

(declaim (inline nan-p))
(defun nan-p (x)
  (df-not-a-number-p x))

(declaim (inline snan-p))
(defun snan-p (x)
  (and (df-not-a-number-p x)
       (df-infinity-p x)))

(declaim (inline qnan-p))
(defun qnan-p (x)
  (and (df-not-a-number-p x)
       (not (df-infinity-p x))))

(declaim (inline subnormal-p))
(defun subnormal-p (x)
  (and (df-subnormal-p x)
       (not (df-zerop x))))

(declaim (inline normal-p))
(defun normal-p (x)
  (and (df-normal-p x)))

(declaim (inline zero-p))
(defun zero-p (x)
  (and (df-normal-p x)
       (df-zerop x)))

(declaim (inline signed-p))
(defun signed-p (x)
  (df-negative-p x))

(declaim (inline canonical-p))
(defun canonical-p (x)
  (decimal-float-p x))

#+nil
(defun shift (x digits &optional (copy-p t))
  (if (finite-p x)
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
