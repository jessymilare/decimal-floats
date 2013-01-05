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
       (not (df-zero-p x))))

(declaim (inline normal-p))
(defun normal-p (x)
  (df-normal-p x))

(declaim (inline zero-p))
(defun zero-p (x)
  (df-zero-p x))

(declaim (inline signed-p))
(defun signed-p (x)
  (df-negative-p x))

(declaim (inline canonical-p))
(defun canonical-p (x)
  (check-type x decimal-float)
  t)

(declaim (inline canonical))
(defun canonical (x)
  (check-type x decimal-float)
  x)

(declaim (inline radix))
(defun radix (x)
  (check-type x decimal-float)
  +base+)

(declaim (inline decimal-class))
(defun decimal-class (x)
  (with-inf-nan-handler (x :+infinity :+infinity
                           :-infinity :-infinity
                           :qnan :nan
                           :snan :snan)
    (cond
      ((df-zero-p x) (if (df-negative-p x) :-zero :+zero))
      ((df-subnormal-p x) (if (df-negative-p x) :-subnormal :+subnormal))
      ((df-negative-p x) :-normal)
      (t :+normal))))

(declaim (inline decimal-class-string))
(defun decimal-class-string (x)
  (with-inf-nan-handler (x :+infinity "+Infinity"
                           :-infinity "-Infinity"
                           :qnan "NaN"
                           :snan "sNaN")
    (cond
      ((df-zero-p x) (if (df-negative-p x) "-Zero" "+Zero"))
      ((df-subnormal-p x) (if (df-negative-p x) "-Subnormal" "+Subnormal"))
      ((df-negative-p x) "-Normal")
      (t "+Normal"))))

(declaim (inline copy-abs))
(defun copy-abs (x)
  (if (df-negative-p x)
      (let ((y (copy-decimal x)))
        (setf (df-negative-p y) nil)
        y)
      x))

(declaim (inline copy-sign))
(defun copy-sign (x y)
  (if (eq (df-negative-p x) (df-negative-p y))
      x
      (let ((z (copy-decimal x)))
        (setf (df-negative-p z) (df-negative-p y))
        z)))

(declaim (inline copy-negate))
(defun copy-negate (x)
  (let ((y (copy-decimal x)))
    (setf (df-negative-p y) (not (df-negative-p x)))
    y))

(declaim (inline same-quantum-p))
(defun same-quantum-p (x y)
  (with-inf-nan-handler (x :nan (df-not-a-number-p y)
                           :infinity (and (not (df-not-a-number-p y))
                                          (df-infinity-p y)))
    (with-inf-nan-handler (y :any nil)
      (= (df-exponent x) (df-exponent y)))))
