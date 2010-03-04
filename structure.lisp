;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(locally
    (declare #+sbcl (optimize sb-ext:inhibit-warnings))
  (defconstant +word-bits+
    (or
     #+sbcl sb-vm:n-word-bits
     #+(or x86-64 ppc64 ia64) 64
     #+(or x86 ppc sparc alpha mips hppa) 32
     ;; default: guesses the architecture
     (if (> most-positive-fixnum (expt 2 32))
         64
         32))))

(defconstant +decimal-slot-bits+
  (or #+clisp 32 ; clisp doesn't seem to support (unsigned-byte 64) arrays
      +word-bits+))

(defconstant +decimal-slot-digits+
  (truncate (log (expt 2 +decimal-slot-bits+) 10)))

(defconstant +num-of-digits-bits+ (integer-length (1- +decimal-slot-digits+))
  "Number of bits needed to store one number from 0 to +decimal-slot-digits+ - 1.")

;;; === (defconstant +internal-e-max+ most-positive-fixnum) most of the times
(defconstant +internal-e-max+
  (min most-positive-fixnum (- 1 most-negative-fixnum))) ; this and the one below must fit in a fixnum

;;; === (defconstant +internal-e-min+ most-negative-fixnum) most of the times
(defconstant +internal-e-min+
  (- 1 +internal-e-max+))

(defconstant +maximum-exponent+
  (+ (* +internal-e-max+ +decimal-slot-digits+) ; quantity stored in df-iexponent
     (1- +decimal-slot-digits+)) ; digits of the most significant slot (last digit)
  "Maximum value of the 'adjusted exponent', i.e. the maximum
 exponent of any number when it is in scientific notation.
 This is the constant Emax of the specification.")

(defconstant +minimum-exponent+ (- 1 +maximum-exponent+)
  "Minimum value of the 'adjusted exponent' for any normal number,
 i.e. the minimum exponent of a normal number when it is in exponencial notation.
 A subnormal number might have a smaller adjusted exponent.
 This is the constant Emin of the specification.")

(defconstant +maximum-precision+
  (min (* +decimal-slot-digits+ (floor (1- array-dimension-limit) 2)) ; ideal value
       most-positive-fixnum              ; precision must always be a fixnum
       (truncate +maximum-exponent+ 10)) ; maximum recommended by spec divided by 2.
  ;; The constant +maximum-exponent+ is already very big (compared to IEEE 754 decimal arithmetics),
  ;; so this restriction shouldn't be even noticed (specially for 64-bit systems, since a precision 
  ;; this big will can't be used unless you have 2^65 or more bytes of RAM).
  "Maximum value allowed for *PRECISION*, or maximum length of any number, in digits.")

(defconstant +minimum-precision+ 1
  "Minimum value allowed for *PRECISION*.
This variable is just for documentation.")

(deftype unsigned-word ()
  `(unsigned-byte ,+word-bits+))

(deftype decimal-slot ()
  `(mod ,(expt 10 +decimal-slot-digits+)))

(deftype slot-array (&optional (length '*))
  `(simple-array decimal-slot (,length)))

(deftype slot-array-length ()
  `(integer 0 ,(ceiling +maximum-precision+ +decimal-slot-digits+)))

(defstruct (decimal-float
	     (:conc-name df-)
	     (:constructor %make-df (slots iexponent))
	     (:copier copy-df))
  (extra 0 :type #.`(unsigned-byte ,(+ 4 (* 2 +num-of-digits-bits+))))
  (slots 1 :type (or null (simple-array decimal-slot)))
  (iexponent 0 :type (or fixnum condition)))

(macrolet ((def-field (accessor start end &optional doc)
	     `(progn (defun ,accessor (x)
		       ,@(if doc (list doc))
		       (ldb (byte (- ,end ,start) ,start) (df-extra x)))
		     (defun (setf ,accessor) (value x)
		       ,@(if doc (list doc))
		       (setf (ldb (byte (- ,end ,start) ,start) (df-extra x))
			     value))))
	   (def-bitp (accessor bit &optional doc)
	     `(progn (defun ,accessor (x)
		       ,@(if doc (list doc))
		       (logbitp ,bit (df-extra x)))
		     (defun (setf ,accessor) (value x)
		       (setf (logbitp ,bit (df-extra x)) value)))))
  ;; Meanings of the EXTRA slot bits
  (def-bitp  df-negative-p     0)
  (def-bitp  df-infinity-p     1)
  (def-bitp  df-not-a-number-p 2)
  (def-bitp  df-subnormal-p    3)

  ;; About first-slot-last-digit and last-slot-first-digit:

  ;; We store the slots (one slot is a digit in base 10 ^ +decimal-slot-digits+)
  ;; in little-endian order, and, therefore, the last slot is the most significant one.

  ;; The first (decimal) digit of the last slot is the position
  ;; (counting from the right, starting in 0)
  ;; of the most significant valid digit of that slot
  ;; (and, therefore, the most significant digit of the number).
  ;; The digits of the last slot which are more significant
  ;; than fsld are unused and meant to be always zero.

  ;; Likewise, the last digit of the first slot is the position
  ;; (also from the right counting from 0)
  ;; of the least significant valid digit of the first slot
  ;; (and, therefore, the least significant digit of the number).
  ;; The digits of the first slot which are more significant
  ;; than lsfd are also unused and meant to be always zero.
  (def-field df-first-slot-last-digit 4 (+ 4 +num-of-digits-bits+))
  (def-field df-last-slot-first-digit  (+ 4 +num-of-digits-bits+)
	     (+ 4 (* 2 +num-of-digits-bits+))))

(macrolet ((def (name (x &rest vars)
                      &body body)
             (let ((%name (symbolicate "%DF-" name))
                   (df-name (symbolicate "DF-" name)))
               `(progn
                  (defun ,%name ,(mapcar #'first vars)
                    ,@body)
                  (defun ,df-name (,x)
                    (,%name ,@(mapcar #'second vars)))))))
  (def zerop (x (iexponent (df-iexponent x))
                (slots (df-slots x)))
    (if (= iexponent +minimum-exponent+)
        (every #'zerop slots)
        (zerop (aref slots (1- (length slots))))))

  #+nil
  (def subnormal-p (x (iexponent (df-iexponent x))
                      (slots (df-slots x)))
    (and (= iexponent +minimum-exponent+)
         (zerop (aref slots (1- (length slots))))))

  (def finite-p (x (extra (df-extra x)))
    (zerop (ldb (byte 2 1) extra)))

  (def normal-p (x (extra (df-extra x)))
    (zerop (ldb (byte 3 1) extra)))

  (def count-digits (x (length (length (df-slots x)))
                       (fsld (df-first-slot-last-digit x))
                       (lsfd (df-last-slot-first-digit x)))
    (+ (*  +decimal-slot-digits+ (- length 1))
       (1+ lsfd)
       (-  fsld)))

  (def logb (x (iexponent (df-iexponent x))
               (lsfd (df-last-slot-first-digit x)))
    (+ (* +decimal-slot-digits+ iexponent)
       lsfd))

  (def exponent (x (iexponent (df-iexponent x))
                   (length (length (df-slots x)))
                   (fsld (df-first-slot-last-digit x)))
    (+ (* +decimal-slot-digits+ (+ iexponent (- 1 length)))
       fsld)))

(defun make-digits-array (slots)
  (make-array slots :element-type 'decimal-slot
	      :initial-element 0))

(define-constant +expt-10+
    ;; Powers of 10 for fast reference.
    (let ((array (make-array (1+ +decimal-slot-digits+)
			     :element-type `(integer 0 ,(expt 10 +decimal-slot-digits+)))))
      (loop for i from 0 to +decimal-slot-digits+
	 for pow = 1 then (* pow 10) do
	   (setf (aref array i) pow))
      array)
  :test #'equalp)

(defun first-digit-of-slot (number)
  ;; Used to calculate lsfd of a decimal-float.
  (declare (decimal-slot number))
  (do* ((low 0)
        (high +decimal-slot-digits+)
        (mid (truncate (+ low high) 2) (truncate (+ low high) 2)))
       ((>= low mid) low)
    (if (< number (aref +expt-10+ mid))
        (setf high mid)
        (setf low  mid))))

(defun last-digit-of-slot (number)
  ;; Used to calculate fsld of a decimal-float.
  (declare (decimal-slot number))
  (if (zerop number)
      0
      (do* ((low 0)
            (high +decimal-slot-digits+)
            (mid (truncate (+ low high) 2) (truncate (+ low high) 2)))
           ((>= low mid) low)
        (if (zerop (rem number (aref +expt-10+ mid)))
            (setf low  mid)
            (setf high mid)))))

(defun map-digits-array (function x-slots fsld lsfd &optional update-p)
  ;; Maps the digits, from most significant one to least significant one
  ;; and changes them in-place unless update-p is nil.
  (let ((last-slot (1- (length x-slots))))
    (loop
       for nslot from last-slot downto 0
       for slot = (aref x-slots nslot)
       for start = lsfd then (1- +decimal-slot-digits+)
       for end   = (if (= nslot 0)
                       fsld
                       0)
       for acc = 0
       do
         (loop
            for i from start downto end
            do
            (multiple-value-bind (next-digit next-slot)
                (truncate slot (aref +expt-10+ i))
              (declare ((mod 10) next-digit))
              (if update-p
                  (setf acc (+ (* acc 10)
                               (the (mod 10) (funcall function next-digit))))
                  (funcall function next-digit))
              (setf slot next-slot)))
         (when update-p
           (setf (aref x-slots nslot) (if (zerop nslot)
                                          (* acc (aref +expt-10+ end))
                                          acc)))))
  x-slots)

(defun calculate-info (digits printed-exp dot-position)
  (let ((adj-exponent (+ printed-exp (- dot-position 1))))
    (multiple-value-bind (df-iexponent lsfd) (floor adj-exponent +decimal-slot-digits+)
      (multiple-value-bind (slots-1 fsld) (ceiling (- digits lsfd 1) +decimal-slot-digits+)
        (values (make-digits-array (1+ slots-1)) (- fsld) lsfd df-iexponent)))))

(defun parse-info (x)
  ;; Abstracting away differences between normal and subnormal numbers information
  (let ((digits (df-count-digits x))
        (exponent (df-exponent x))
        (adj-exponent (df-logb x))
        (x-slots (df-slots x))
        (fsld (df-first-slot-last-digit x))
        (lsfd (df-last-slot-first-digit x)))
    (if (df-subnormal-p x)
        (let* ((last-position (or (position-if #'plusp x-slots) 0))
               (discarded-slots (- (length x-slots) (1+ last-position)))
               (new-lsfd (first-digit-of-slot (aref x-slots last-position)))
               (discarded-digits (- (+ (* discarded-slots +decimal-slot-digits+)
                                       lsfd)
                                    new-lsfd)))
          (values (- digits discarded-digits) exponent (- adj-exponent discarded-digits)
                  (subseq x-slots 0 (1+ last-position)) fsld new-lsfd))
        (values digits exponent adj-exponent x-slots fsld lsfd))))

(declaim (inline make-decimal-float))

(defun make-decimal-float (iexponent slots &key (negative-p nil) (infinity-p nil)
                           (not-a-number-p nil) (subnormal-p nil)
                           (first-slot-last-digit 0)
                           (last-slot-first-digit 0))
  (let ((x (%make-df slots iexponent)))
    (setf (df-negative-p x) negative-p
          (df-infinity-p x) infinity-p
          (df-not-a-number-p x) not-a-number-p
          (df-subnormal-p x) subnormal-p
          (df-first-slot-last-digit x) first-slot-last-digit
          (df-last-slot-first-digit x) last-slot-first-digit)
    x))
