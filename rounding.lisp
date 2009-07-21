;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (inline set-precision get-precision parse-precision))

(declaim (type (integer 0 #.+maximum-precision+) *precision*))

(defvar *precision* +decimal-slot-digits+
  "The current precision that is being used, i.e., the maximum number of (decimal) digits that
 a number that results from one of the arithmetics operations (except copy operations) will have.
 Any number with more digits will be rounded.")

(def-customize-function (rounding-mode :round-half-up) (discarded-digits last-digits signed-p)
  "The rounding mode to be used. See documentation for FIND-ROUNDING-MODE."
  "This function takes a keyword and returns the corresponding rounding mode function.
 Accepted keywords are :round-half-up, :round-half-down, :round-down,
 :round-up, :round-half-even, :round-ceiling and :round-floor.

 See http://speleotrove.com/decimal/damodel.html#refround for details.

 Information on how to create a rounding mode function is in the file \"doc/rounding-mode\"."
  (declare (type (mod 10) discarded-digits)
           (type (unsigned-byte 12) last-digits)
           (type boolean signed-p)
           (optimize speed (space 0)))
  (:round-half-up (>= discarded-digits 5))
  (:round-half-down (> discarded-digits 5))
  (:round-down nil)
  (:round-up (plusp discarded-digits))
  (:round-half-even (cond
                      ((> discarded-digits 5) t)
                      ((= discarded-digits 5) (oddp last-digits))))
  (:round-ceiling (and (not signed-p) (plusp discarded-digits)))
  (:round-floor (and signed-p (plusp discarded-digits)))
  (:round-05-up (case (rem last-digits 10)
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

(defun %next-up (x))

(defun round-normal-number (delta df-exponent slots fsld discarded-digits signed-p LOCAL-ERROR)
  (let* ((length (+ (length slots) delta))
         (precision *precision*)
         (start 0)
         (lsfd (if (plusp delta)
                   1
                   (first-digit-of-slot (aref slots (1- length))))))
    (when (> (%count-digits length fsld lsfd) precision)
      (multiple-value-bind (new-length new-fsld) (ceiling (- precision 1 lsfd) +decimal-slot-digits+)
        (setf fsld new-fsld
              start (- length new-length)
              length new-length))
      (let ((undecided-p t))
        (multiple-value-bind (last-discarded pow-10)
            (if (zerop fsld)
                (values (1- start) (expt 10 +decimal-slot-digits+))
                (values start (aref +expt-10+ fsld)))
          (setf discarded-digits
                (let ((half-slot (truncate pow-10 2))
                      (slot (rem (aref slots last-discarded) pow-10)))
                  (cond
                    ((zerop slot)
                     (if (zerop discarded-digits)
                         0
                         (progn
                           (setf undecided-p nil)
                           7)))
                    ((< slot half-slot)
                     (setf undecided-p nil)
                     2)
                    ((> slot half-slot)
                     (setf undecided-p nil)
                     7)
                    (t ;; (= next-slot half-slot)
                     (if (zerop discarded-digits)
                         5
                         (progn (setf undecided-p nil)
                                7))))))
          (if (and undecided-p (plusp last-discarded)
                   (find-if #'plusp slots :end last-discarded))
              (incf discarded-digits 2))))
      (decf (aref slots 0) (rem (aref slots 0) (aref +expt-10+ fsld))))
    (cond
      ((zerop delta)
       (when (plusp start)
         (setf slots (subseq slots start))))
      ((plusp delta)
       (setf slots (adjust-array (if (plusp start)
                                     (subseq slots start)
                                     slots)
                                 new-length :initial-element 0)))
      (t
       (setf slots (subseq slots start new-length))))
    ;; FIXME !!!
    (let* ((zerop (every #'zerop slots))
           (x (%make-df slots +internal-e-min+)))
      (setf (df-negative-p x) signed-p
            ;; (df-last-slot-first-digit x) is already 0 by default
            (df-first-slot-last-digit x) fsld
            (df-subnormal-p x) t)
      ;; Attention: order of signalization must be preserved.
      (cond ((plusp discarded-digits)
             (funcall LOCAL-ERROR '$underflow x)
             (funcall LOCAL-ERROR '$subnormal x)
             (funcall LOCAL-ERROR '$inexact x))
            (t (funcall LOCAL-ERROR '$subnormal x)))
      (funcall LOCAL-ERROR '$rounded x)
      (if zerop
          (funcall LOCAL-ERROR '$clamped x)
          x))))

(defun round-overflowed-number (delta slots fsld discarded-digits signed-p LOCAL-ERROR)
  (let* ((new-length (1+ (or (position-if #'plusp ,slots :from-end t)
                             0)))
         (delta-length (- (length slots) new-length)))
    (if (> delta delta-length)
        (let ((x (if (funcall *rounding-mode* 9 0 ,signed-p)
                     (make-infinity ,signed-p)
                     (make-almost-infinity ,signed-p))))
          (funcall LOCAL-ERROR '$overflow x)
          (funcall LOCAL-ERROR '$inexact x)
          (funcall LOCAL-ERROR '$rounded x))
        (round-normal-number delta-length (- +internal-e-max+ (- delta-length delta))
                             (adjust-array slots new-length)
                             fsld discarded-digits signed-p LOCAL-ERROR))))

(defmacro normalize-number (df-iexponent slots fsld discarded-digits signed-p)
  (with-gensyms (delta new-length new-df-iexponent)
    (once-only (df-iexponent slots fsld lsfd discarded-digits signed-p)
      `(cond
         ((> ,df-iexponent +internal-e-max+)
          (round-overflowed-number (- ,df-iexponent +internal-e-max+)
                                 slots fsld discarded-digits signed-p #'LOCAL-ERROR))
         ((< ,df-iexponent +internal-e-min+)
          (round-normal-number (- +internal-e-min+ ,df-iexponent) +internal-e-min+
                               slots fsld discarded-digits signed-p #'LOCAL-ERROR))
         ((zerop (aref ,slots (1- (length ,slots))))
          (let* ((,new-length (1+ (or (position-if #'plusp ,slots :from-end t)
                                      0)))
                 (,delta (- (length ,slots) ,new-length))
                 (,new-df-iexponent (- ,df-iexponent ,delta)))
            (when (< ,new-df-iexponent +internal-e-min+)
              (setf ,delta (+ ,df-iexponent +internal-e-max+)
                    ,new-df-iexponent (- +internal-e-max+)
                    ,new-length (- (length ,slots) ,delta)))
            (values ,new-df-iexponent
                    (adjust-array ,slots ,new-length)
                    ,new-length)))
         (t (round-normal-number 0 df-iexponent slots fsld discarded-digits signed-p))))))
