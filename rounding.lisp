;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (type precision *precision*))

(defvar *precision* (* 2 +decimal-slot-digits+)
  "The current precision that is being used, i.e., the maximum number of
decimal digits that a number that results from one of the arithmetics
operations (except copy operations) will have. Any number with more digits
will be rounded.")

(defvar *rounding-mode* :round-half-up
  "The rounding mode to be used. It must be either a keyword or a function.

Accepted keywords are :ROUND-HALF-UP, :ROUND-HALF-DOWN, :ROUND-DOWN,
:ROUND-UP, :ROUND-HALF-EVEN, :ROUND-CEILING and :ROUND-FLOOR.

See http://speleotrove.com/decimal/damodel.html#refround for details.

You can create a custom rounding mode by using a function.
Information on how to create a rounding mode function is in the file
\"doc/rounding-mode\".")

(def-customize-function find-rounding-mode
    (discarded-digits last-digit signed-p)
  (declare (type (mod 11) discarded-digits)
           (type (unsigned-byte 12) last-digit)
           (type boolean signed-p))
  (:round-half-up (>= discarded-digits 5))
  (:round-half-down (> discarded-digits 5))
  (:round-down nil)
  (:round-up (plusp discarded-digits))
  (:round-half-even (cond
                      ((> discarded-digits 5) t)
                      ((= discarded-digits 5) (oddp last-digit))))
  (:round-ceiling (and (not signed-p) (plusp discarded-digits)))
  (:round-floor (and signed-p (plusp discarded-digits)))
  (:round-05-up (case last-digit
                  (0 (plusp discarded-digits))
                  (5 (plusp discarded-digits)))))

(defun make-almost-infinity (signed-p)
  (multiple-value-bind (n-slots fsld) (ceiling *precision* +decimal-slot-digits+)
    (let* ((fsld (- fsld))
           (slots (make-digits-array n-slots :initial-element (1- +maximum-decimal-slot+)))
           (x (%make-df slots +internal-e-max+)))
      (decf (aref slots 0) (rem (aref slots 0) (aref +expt-10+ fsld)))
      (setf (df-negative-p x) signed-p
            (df-first-slot-last-digit x) fsld
            (df-last-slot-first-digit x) (1- +decimal-slot-digits+))
      x)))

(defun %next-up (iexponent slots fsld lsfd)
  (flet ((%add (x y)
           (declare (type decimal-slot x y)
                    (values bit decimal-slot &optional))
           (let ((value (+ x y)))
             (declare (type (integer 0 #.+maximum-decimal-slot+) value))
             (if (= value +maximum-decimal-slot+)
                 (values 1 0)
                 (values 0 value)))))
    (declare (inline %add))
    (let* ((aux (aref +expt-10+ fsld))
           (end (length slots)))
      (dotimes (i end)
        (setf (values aux (aref slots i))
              (%add (aref slots i) aux))
        (when (zerop aux)
          (return)))
      (let ((last-slot (aref slots (1- end))))
        ;; This test means
        ;; (/= lsfd (first-digit-of-slot last-slot))
        (if (zerop (rem last-slot (aref +expt-10+ (1+ lsfd))))
            (progn
              (if (= +decimal-slot-digits+ (incf fsld))
                  (setf fsld 0))
              (if (= +decimal-slot-digits+ (incf lsfd))
                  (if (= iexponent +internal-e-max+)
                      ;; Overflow
                      (values iexponent slots fsld lsfd t)
                      (let ((slots (%resize-slots slots (if (zerop fsld) 1 0)
                                                  (1+ end))))
                        (when (zerop lsfd)
                          ;; aux should be 1 here, or algorithm is wrong
                          (assert (= 1 aux))
                          (setf (aref slots end) 1))
                        (values (1+ iexponent) slots fsld 0)))
                  (let ((slots (if (zerop fsld)
                                   (%resize-slots slots 1 end)
                                   slots)))
                    (values iexponent slots fsld lsfd))))
            (values iexponent slots fsld lsfd))))))

(defun %next-down (iexponent slots fsld lsfd)
  (flet ((%sub (x y)
           (declare (type decimal-slot x y)
                    (values bit decimal-slot &optional))
           (if (<= y x)
               (values 0 (- x y))
               (values 1 (- +maximum-decimal-slot+
                            (the decimal-slot (- y x)))))))
    (declare (inline %sub))
    (let* ((aux (aref +expt-10+ fsld))
           (end (length slots)))
      (dotimes (i end)
        (setf (values aux (aref slots i))
              (%sub (aref slots i) aux))
        (when (zerop aux)
          (return)))
      ;; Non-zero aux here means a zero was received.
      (assert (zerop aux))
      (let ((last-slot (aref slots (1- end))))
        ;; This test means
        ;; (and (> iexponent +internal-e-min+ )
        ;;      (or (zerop last-slot)
        ;;          (/= lsfd (first-digit-of-slot last-slot))))
        (if (zerop (truncate last-slot (aref +expt-10+ lsfd)))
            (if (= -1 (decf lsfd))
                (if (= iexponent +internal-e-min+)
                    ;; Subnormal -- nothing to do
                    (values iexponent slots fsld 0 t)
                    (let ((slots (%resize-slots
                                  slots
                                  (if (= -1 (decf fsld))
                                      (progn (setf fsld (1- +decimal-slot-digits+)) -1)
                                      0)
                                  (1- end))))
                      (incf (aref slots 0) (* (1- +base+) (aref +expt-10+ fsld)))
                      (values (1- iexponent) slots fsld (1- +decimal-slot-digits+))))
                (let ((slots (if (= -1 (decf fsld))
                                 (progn (setf fsld (1- +decimal-slot-digits+))
                                        (%resize-slots slots -1 end))
                                 slots)))
                  (incf (aref slots 0) (* (1- +base+) (aref +expt-10+ fsld)))
                  (values iexponent slots fsld lsfd)))
            (values iexponent slots fsld lsfd))))))

(defun coerce-to-precision-digits (end slots fsld lsfd)
  (let ((lsfd (if (= end 1) (max lsfd fsld) lsfd))
        (precision *precision*))
    (cond
      ((> (%df-count-digits end fsld lsfd) precision)
       (multiple-value-bind (new-length new-fsld)
           (ceiling (- precision 1 lsfd) +decimal-slot-digits+)
         (let ((start (- end (1+ new-length)))
               (fsld (- new-fsld))
               (discarded-digits 0))
           (multiple-value-bind (last-discarded pow-10)
               (if (zerop fsld)
                   (values (1- start) +maximum-decimal-slot+)
                   (values start (aref +expt-10+ fsld)))
             (setf discarded-digits
                   (let* ((half-pow-10 (truncate pow-10 2))
                          (discarded-slot (rem (aref slots last-discarded) pow-10))
                          (discarded-digits
                           ;; Value that will be used by rounding-mode
                           ;; (see doc/rounding-mode)
                           (cond
                             ((< discarded-slot half-pow-10)
                              (if (or (plusp discarded-slot)
                                      (find-if #'plusp slots :end last-discarded))
                                  2
                                  0))
                             ((> discarded-slot half-pow-10)
                              7)
                             (t ;; (= slot half-slot)
                              (if (find-if #'plusp slots :end last-discarded)
                                  7
                                  5)))))
                     ;; Setting invalid digits to zero
                     (decf (aref slots last-discarded) discarded-slot)
                     (values (resize-slots slots start end) fsld lsfd
                             discarded-digits t)))))))
      (t (values (resize-slots slots 0 end) fsld lsfd 0 nil)))))

(defun round-number (end iexponent slots fsld signed-p)
  (let ((lsfd (cond
                ((> end (length slots)) 0)
                ((= end 1)
                 ;; Make sure that the exponent of a zero is not lost:
                 (max (first-digit-of-slot (aref slots 0))
                      fsld))
                (t (first-digit-of-slot (aref slots (1- end)))))))
    (multiple-value-bind (slots fsld lsfd discarded-digits some-digit-discarded-p)
        (coerce-to-precision-digits end slots fsld lsfd)
      (when (find-rounding-mode *rounding-mode* discarded-digits
                                (rem (truncate (aref slots 0)
                                               (aref +expt-10+ fsld))
                                     10)
                                signed-p)
        (let (infinite-p)
          (setf (values iexponent slots fsld lsfd infinite-p)
                (%next-up iexponent slots fsld lsfd))
          (when infinite-p
            (return-from round-number
              ;; Overflow
              (decimal-error-cond ((make-infinity signed-p))
                decimal-overflow decimal-inexact decimal-rounded)))))
      (let ((x (make-decimal-float iexponent slots :negative-p signed-p
                                   :last-slot-first-digit lsfd
                                   :first-slot-last-digit fsld)))
        (cond
          ((and (= +internal-e-min+ iexponent)
                (zerop (aref slots (1- (length slots)))))
           ;; Result is subnormal (internally at least, since zero is
           ;; not subnormal by definition).
           (setf (df-subnormal-p x) t)
           (if (plusp discarded-digits)
               ;; Underflow
               (decimal-error-cond (x)
                 decimal-underflow decimal-subnormal decimal-inexact decimal-rounded
                 ((every #'zerop slots) decimal-clamped))
               (decimal-error-cond (x)
                 decimal-subnormal
                 (some-digit-discarded-p decimal-rounded))))
          (some-digit-discarded-p
           (decimal-error-cond (x)
             ((plusp discarded-digits) decimal-inexact) decimal-rounded))
          (t x))))))

(defun normalize-number (iexponent slots fsld signed-p)
  (let* ((length (length slots))
         (new-length (1+ (or (position-if #'plusp slots :from-end t)
                             0)))
         (iexponent (+ iexponent (- new-length length))))
    (cond
      ((> iexponent +internal-e-max+)
       (decimal-error-cond
           ((if (find-rounding-mode *rounding-mode* 10 10 signed-p)
                (make-infinity signed-p)
                (make-almost-infinity signed-p)))
         ;; Order of signalization must be preserved.
         decimal-overflow decimal-inexact decimal-rounded))
      ((<= iexponent +internal-e-min+)
       (round-number (+ new-length (- +internal-e-min+ iexponent))
                     +internal-e-min+ slots fsld signed-p))
      (t (round-number new-length iexponent slots fsld signed-p)))))
