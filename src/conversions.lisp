;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2013 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defun integer-to-decimal (integer &key round-p)
  (with-operation (integer-to-decimal integer)
    (let ((abs-integer (abs integer)))
      (cond
        ;; Assuming INTEGER is probably a fixnum
        ((< abs-integer +maximum-decimal-slot+)
         (let ((slots (make-digits-array 1 :initial-element abs-integer)))
           (if round-p
               (round-number 1 0 slots 0 (minusp integer))
               (make-decimal-float 0 slots
                                   :negative-p (minusp integer)
                                   :last-slot-first-digit (first-digit-of-slot integer)))))
        ((< abs-integer (expt +maximum-decimal-slot+ 2))
         (multiple-value-bind (s1 s0) (truncate abs-integer +maximum-decimal-slot+)
           (let ((slots (make-digits-array 2)))
             (setf (aref slots 0) s0
                   (aref slots 1) s1)
             (if round-p
                 (round-number 2 1 slots 0 (minusp integer))
                 (make-decimal-float 1 slots :negative-p (minusp integer)
                                     :last-slot-first-digit (first-digit-of-slot s1))))))
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
             (if round-p
                 (round-number length (1- length) slots 0 (minusp integer))
                 (make-decimal-float (1- length) slots :negative-p (minusp integer)
                                     :last-slot-first-digit
                                     (first-digit-of-slot slot))))))))))

(defun %decimal-to-integer (x)
  (let* ((slots (df-slots x))
         (length (length slots))
         (iexponent (df-iexponent x))
         (fsld (df-first-slot-last-digit x))
         (exponent (%df-exponent iexponent length fsld)))
    (unless (zerop exponent)
      (decimal-error-cond (++qnan+ :return-p t :correctable-p t)
        (decimal-invalid-operation))
      (if (minusp iexponent)
          (return-from %decimal-to-integer 0)))
    (let ((start (- length 1 iexponent))
          (first-pow 1))
      (if (< start 0)
          (setf first-pow (expt +maximum-decimal-slot+ (- start))
                start 0))
      (let ((result (loop for i from start below length
                       for pow = first-pow then (* pow +maximum-decimal-slot+)
                       summing (* pow (aref slots i)))))
        (if (df-negative-p x) (- result) result)))))

(defun decimal-to-integer (x)
  (with-operation (decimal-to-integer x)
    (with-inf-nan-handler (x :any (decimal-error-cond (x)
                                    (decimal-invalid-operation)))
      (%decimal-to-integer x))))

(defun float-to-decimal (float)
  (declare (type (or float keyword) float))
  (if (keywordp float)
      (ecase float
        (:+infinity ++infinity+)
        (:-infinity +-infinity+)
        (:nan ++qnan+))
      (cond
        #+(or sbcl ccl ecl allegro)
        (#+sbcl (sb-ext:float-infinity-p float)
                #+ccl (ccl::infinity-p float)
                #+ecl (ext:float-infinity-p float)
                #+allegro (excl::infinityp)
                (if (minusp float) +-infinity+ ++infinity+))
        #+sbcl ((sb-ext:float-trapping-nan-p float)
                ++snan+)
        (#+sbcl (sb-ext:float-nan-p float)
                #+ccl (ccl::nan-or-infinity-p float)
                #+ecl (ext:float-nan-p float)
                #+allegro (excl::nan-p float)
                #-(or sbcl ccl allegro) (/= float float)
                ++qnan+)
        (t (parse-decimal
            (substitute-if #\e (rcurry #'find "sfdl") (princ-to-string float)))))))

#+ (or sbcl ecl)
(defun make-lisp-qnan (float)
  (#+sbcl sb-int:with-float-traps-masked
          #+sbcl (:overflow :invalid :divide-by-zero)
          #+ecl progn
    (/ (float 0.0 float) (float 0.0 float))))

(defun decimal-to-float (x &optional
                         (other (let ((precision *precision*))
                                  #+sbcl (declare (optimize sb-ext:inhibit-warnings))
                                  (cond
                                    ((<= precision 24) 1f0)
                                    ((<= precision 53) 1d0)
                                    (t 1l0)))))
  (with-operation (decimal-to-float x other)
    (with-inf-nan-handler
        (x
         :nan
         #+(or sbcl ecl) (make-lisp-qnan other)
         #-sbcl
         (etypecase other
           (single-float #+ccl 1f+-0
                         #+allegro excl::*nan-single*
                         #-(or allegro ccl) :nan)
           (short-float  #+ccl 1s+-0
                         #-ccl :nan)
           (double-float #+ccl 1d+-0
                         #+allegro excl::*nan-double*
                         #-(or allegro ccl) :nan)
           (long-float   #+ccl 1l+-0
                         #-ccl :nan))
         :+infinity
         (etypecase other
           (single-float #+ccl 1f++0
                         #+sbcl sb-ext:single-float-positive-infinity
                         #+(or ecl cmu abcl scl) ext:single-float-positive-infinity
                         #+allegro excl::*infinity-single*
                         #-(or sbcl ccl ecl cmu abcl scl allegro) :+infinity)
           (short-float  #+ccl 1s++0
                         #+sbcl sb-ext:short-float-positive-infinity
                         #+(or ecl cmu) ext:short-float-positive-infinity
                         #-(or sbcl ccl ecl cmu) :+infinity)
           (double-float #+ccl 1d++0
                         #+sbcl sb-ext:double-float-positive-infinity
                         #+(or ecl cmu abcl scl) ext:double-float-positive-infinity
                         #+allegro excl::*infinity-double*
                         #-(or sbcl ccl ecl cmu abcl scl allegro) :+infinity)
           (long-float   #+ccl 1l++0
                         #+sbcl sb-ext:long-float-positive-infinity
                         #+(or ecl cmu) ext:long-float-positive-infinity
                         #-(or sbcl ecl cmu ccl) :+infinity))
         :-infinity
         (etypecase other
           (single-float #+ccl -1f++0
                         #+sbcl sb-ext:single-float-negative-infinity
                         #+(or ecl cmu abcl scl) ext:single-float-negative-infinity
                         #+allegro excl::*negative-infinity-single*
                         #-(or sbcl ccl ecl cmu abcl scl allegro) :-infinity)
           (short-float  #+ccl -1s++0
                         #+sbcl sb-ext:short-float-negative-infinity
                         #+(or ecl cmu) ext:short-float-negative-infinity
                         #-(or sbcl ccl ecl cmu) :-infinity)
           (double-float #+ccl -1d++0
                         #+sbcl sb-ext:double-float-negative-infinity
                         #+(or ecl cmu abcl scl) ext:double-float-negative-infinity
                         #+allegro excl::*negative-infinity-double*
                         #-(or sbcl ccl ecl cmu abcl scl allegro) :-infinity)
           (long-float   #+ccl -1l++0
                         #+sbcl sb-ext:long-float-negative-infinity
                         #+(or ecl cmu) ext:long-float-negative-infinity
                         #-(or sbcl ccl ecl cmu) :-infinity)))
      (let* ((slots (df-slots x))
             (length (length slots))
             (iexponent (df-iexponent x))
             (n-slots
              ;; Based on IEE 754 precision, one extra digit
              (etypecase other
                ;; Single-precision floating point
                ((or short-float single-float) (truncate 25 +decimal-slot-digits+))
                ;; Double-precision floating point
                (double-float (truncate 54 +decimal-slot-digits+))
                ;; Quadruple-precision floating-point
                (long-float (truncate 114 +decimal-slot-digits+))))
             (start (max 0 (- length n-slots))))
        (* (expt (float +maximum-decimal-slot+ other)
                 (- iexponent (- length start 1)))
           (loop for i from start below length
              for pow = 1 then (* pow +maximum-decimal-slot+)
              summing (* pow (aref slots i))))))))
