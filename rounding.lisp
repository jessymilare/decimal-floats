;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (type (integer 0 #.+maximum-precision+) *precision*))

(defvar *precision* (* 2 +decimal-slot-digits+)
  "The current precision that is being used, i.e., the maximum number of (decimal) digits that
 a number that results from one of the arithmetics operations (except copy operations) will have.
 Any number with more digits will be rounded.")

(def-customize-function (rounding-mode :round-half-up) (discarded-digits last-digit signed-p)
  "The rounding mode to be used. See documentation for FIND-ROUNDING-MODE."
  "This function takes a keyword and returns the corresponding rounding mode function.
 Accepted keywords are :round-half-up, :round-half-down, :round-down,
 :round-up, :round-half-even, :round-ceiling and :round-floor.

 See http://speleotrove.com/decimal/damodel.html#refround for details.

 Information on how to create a rounding mode function is in the file \"doc/rounding-mode\"."
  (declare (type (mod 11) discarded-digits)
           (type (unsigned-byte 12) last-digit)
           (type boolean signed-p)
           (optimize speed (space 0)))
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
  (multiple-value-bind (slots fsld) (ceiling *precision* +decimal-slot-digits+)
    (let* ((fsld (- fsld))
           (slots (make-array slots :element-type 'decimal-slot
                              :initial-element (1- (expt 10 +decimal-slot-digits+))))
           (x (%make-df slots +internal-e-max+)))
      (decf (aref slots 0) (rem (aref slots 0) (expt 10 fsld)))
      (setf (df-negative-p x) signed-p
            (df-first-slot-last-digit x) fsld
            (df-last-slot-first-digit x) (1- +decimal-slot-digits+))
      x)))

(defun %next-up (iexponent slots fsld lsfd)
  (flet ((%add (x y)
           (declare (type decimal-slot x y)
                    (values bit decimal-slot &optional))
           (let ((value (+ x y)))
             (declare (type (integer 0 #.(expt 10 +decimal-slot-digits+)) value))
             (if (= value (expt 10 +decimal-slot-digits+))
                 (values 1 0)
                 (values 0 value)))))
    (declare (inline %add))
    (let* ((aux (aref +expt-10+ fsld))
           (end (length slots)))
      (dotimes (i end)
        (setf (values aux (aref slots i))
              (%add (aref slots i) aux)))
      (let ((last-slot (aref slots (1- end))))
        (if (zerop (rem last-slot (aref +expt-10+ (1+ lsfd))))
            ;; This test means
            ;; (/= lsfd (first-digit-of-slot last-slot))
            (let ((start 0))
              (if (= +decimal-slot-digits+ (incf fsld))
                  (setf fsld 0 start 1))
              (if (= +decimal-slot-digits+ (incf lsfd))
                  (setf lsfd 0 end (1+ end)))
              (if (= iexponent +internal-e-max+)
                  ;; Overflow
                  (values iexponent slots fsld lsfd t)
                  (let ((slots (resize-slots slots start end)))
                    (when (zerop lsfd)
                      ;; aux should be 1 here, or algorithm is wrong
                      (assert (= 1 aux))
                      (setf (aref slots (1- end)) aux))
                    (values (1+ iexponent) slots fsld lsfd))))
            (values iexponent slots fsld lsfd))))))

(defun %next-down (iexponent slots fsld lsfd)
  (flet ((%sub (x y)
           (declare (type decimal-slot x y)
                    (values bit decimal-slot &optional))
           (if (<= y x)
               (values 0 (- x y))
               (values 1 (- (expt 10 +decimal-slot-digits+)
                            (the decimal-slot (- y x)))))))
    (declare (inline %sub))
    (let* ((aux (aref +expt-10+ fsld))
           (end (length slots)))
      (dotimes (i end)
        (setf (values aux (aref slots i))
              (%sub (aref slots i) aux)))
      ;; Non-zero aux here means a zero was received.
      (assert (zerop aux))
      (let ((last-slot (aref slots (1- end))))
        (if (and (> iexponent +internal-e-min+) 
                 (zerop (truncate last-slot (aref +expt-10+ lsfd))))
            ;; This test means
            ;; (and (> iexponent +internal-e-min+ )
            ;;      (or (zerop last-slot)
            ;;          (/= lsfd (first-digit-of-slot last-slot))))
            (let ((start 0))
              (if (= -1 (decf fsld))
                  (setf fsld (1- +decimal-slot-digits+) start -1))
              (if (= -1 (decf lsfd))
                  (setf lsfd (1- +decimal-slot-digits+) end (1- end)))
              (let ((slots (resize-slots slots start end)))
                (incf (aref slots 0) (* 9 (aref +expt-10+ fsld)))
                (values (1- iexponent) slots fsld lsfd)))
            (values iexponent slots fsld lsfd))))))

(defun coerce-to-precision-digits (end slots fsld lsfd)
  (when (= end 1)
    (setf lsfd (max lsfd fsld)))
  (let ((start 0)
        (precision *precision*)
        (discarded-digits 0)
        some-digit-discarded-p)
    (when (> (%df-count-digits end fsld lsfd) precision)
      (setf some-digit-discarded-p t)
      (multiple-value-bind (new-length new-fsld) (ceiling (- precision 1 lsfd) +decimal-slot-digits+)
        (setf fsld (- new-fsld)
              start (- end (1+ new-length))))
      (multiple-value-bind (last-discarded pow-10)
          (if (zerop fsld)
              (values (1- start) (expt 10 +decimal-slot-digits+))
              (values start (aref +expt-10+ fsld)))
        (setf discarded-digits
              (let ((half-slot (truncate pow-10 2))
                    (slot (rem (aref slots last-discarded) pow-10)))
                (cond
                  ((zerop slot)
                   (if (find-if #'plusp slots :end last-discarded)
                       2
                       0))
                  ((< slot half-slot)
                   2)
                  ((> slot half-slot)
                   7)
                  (t ;; (= slot half-slot)
                   (if (and (plusp last-discarded)
                            (find-if #'plusp slots :end last-discarded))
                       7
                       5)))))))
    (setf slots (resize-slots slots start end))
    ;; Setting invalid digits to zero
    (decf (aref slots 0) (rem (aref slots 0) (aref +expt-10+ fsld)))
    (values slots fsld lsfd discarded-digits some-digit-discarded-p)))

(defun round-finite-number (end iexponent slots fsld signed-p)
  (let ((lsfd (if (> end (length slots))
                  0
                  (first-digit-of-slot (aref slots (1- end)))))
        (discarded-digits 0)
        some-digit-discarded-p)
    (setf (values slots fsld lsfd discarded-digits some-digit-discarded-p)
          (coerce-to-precision-digits end slots fsld lsfd))
    (when (funcall *rounding-mode* discarded-digits
                   (rem (truncate (aref slots 0) (aref +expt-10+ fsld)) 10)
                   signed-p)
      (let (infinite-p)
        (setf (values iexponent slots fsld lsfd infinite-p)
              (%next-up iexponent slots fsld lsfd))
        (when infinite-p
          (return-from round-finite-number
            ;; Overflow
            (decimal-error-cond ((make-infinity signed-p))
                                decimal-overflow decimal-inexact decimal-rounded)))))
    (let ((x (make-decimal-float iexponent slots :negative-p signed-p
                                 :last-slot-first-digit lsfd :first-slot-last-digit fsld)))
      (cond
        ((and (= +minimum-exponent+ iexponent)
              (zerop (aref slots (1- (length slots)))))
         ;; Result is subnormal (internally at least, since zero is not subnormal by definition).
         (setf (df-subnormal-p x) t)
         (if (plusp discarded-digits)
             ;; Underflow
             (decimal-error-cond (x)
               decimal-underflow decimal-subnormal decimal-inexact
               decimal-rounded
               ((every #'zerop slots) decimal-clamped))
             (decimal-error-cond (x)
               decimal-subnormal
               (some-digit-discarded-p decimal-rounded))))
        (some-digit-discarded-p
         (decimal-error-cond (x)
           ((plusp discarded-digits) decimal-inexact) decimal-rounded))
        (t x)))))

(defun round-overflowed-number (end slots fsld signed-p)
  (let ((new-length (1+ (or (position-if #'plusp slots :from-end t)))))
    (if (> end new-length)
        (decimal-error-cond
            ((if (funcall *rounding-mode* 10 10 signed-p)
                 (make-infinity signed-p)
                 (make-almost-infinity signed-p)))
          ;; Order of signalization must be preserved.
          decimal-overflow decimal-inexact decimal-rounded)
        (round-finite-number new-length (- +internal-e-max+ (- new-length end))
                             (adjust-array slots new-length :initial-element 0)
                             fsld signed-p))))

(defun normalize-number (iexponent slots fsld signed-p)
  (cond
     ((> iexponent +internal-e-max+)
      (round-overflowed-number (+ (length slots) (- +internal-e-max+ iexponent))
                               slots fsld signed-p))
     ((<= iexponent +internal-e-min+)
      (round-finite-number (+ (length slots) (- +internal-e-min+ iexponent))
                           +internal-e-min+
                           slots fsld signed-p))
     ((zerop (aref slots (1- (length slots))))
      (let* ((new-length (1+ (or (position-if #'plusp slots :from-end t)
                                  0)))
             (new-iexponent (+ iexponent (-  new-length (length slots)))))
        (when (< new-iexponent +internal-e-min+)
          (setf new-length (+ new-length (- +internal-e-min+ new-iexponent))
                new-iexponent +internal-e-min+))
        (round-finite-number new-length new-iexponent slots fsld signed-p)))
     (t (round-finite-number (length slots) iexponent slots fsld signed-p))))
