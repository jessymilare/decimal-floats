;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :decimal-floats)

(defconstant +decimal-slot-bits+
  (or
   #+sbcl sb-vm:n-word-bits
   #+clisp 32 ; clisp doesn't seem to support (unsigned-byte 64) arrays
   #+(or x86-64 ppc64 ia64) 64
   #+(or x86 ppc sparc alpha mips hppa) 32
   ;; default: guesses the architecture
   #-sbcl
   (if (> most-positive-fixnum (expt 2 32))
       64
       32))
  "Number of bits that each slot will have.")

(defconstant +decimal-slot-digits+
  (truncate (log (expt 2 +decimal-slot-bits+) 10))
  "Number of digits that each slot will represent.")

(defconstant +num-of-digits-bits+ (integer-length (1- +decimal-slot-digits+))
  "Number of bits needed to store one number from 0 to +decimal-slot-digits+ - 1.")

(defconstant +internal-e-max+
  (min most-positive-fixnum (- most-negative-fixnum))) ; must fit in a fixnum

(defconstant +maximum-exponent+
  (+ (* +internal-e-max+ +decimal-slot-digits+) ; quantity stored in df-exponent
     (1- +decimal-slot-digits+)) ; digits of the most significant slot (last digit)
  "Maximum value of the 'adjusted exponent', i.e. the maximum
 exponent of any number when it is in scientific notation.
 This is the constant Emax of the specification.")

(defconstant +minimum-exponent+ (- 1 +maximum-exponent+) ; this is not the most convenient value, but the spec says...
  "Minimum value of the 'adjusted exponent' for any normal number,
 i.e. the minimum exponent of a normal number when it is in exponencial notation.
 A subnormal number might have a smaller adjusted exponent.
 This is the constant Emin of the specification.")

(defconstant +maximum-precision+
  (min (* +decimal-slot-digits+ (- array-dimension-limit 1))  ; ideal value
       most-positive-fixnum             ; so that the number of digits is always a fixnum
       (truncate +maximum-exponent+ 10)) ; maximum recommended by spec divided by 2.
  ;; The constant +maximum-exponent+ is already very big (compared to normal decimal arithmetics),
  ;; so this restriction shouldn't be even noticed (specially for 64-bit systems, since a precision 
  ;; this big will not be used unless you have 2^65 or more bytes of RAM).
  "Maximum value allowed for *PRECISION*, or maximum length of any number, in digits.
There is no minimum value for *PRECISION*.")

(defconstant +tiny-exponent+ (- +minimum-exponent+ (1- +maximum-precision+))
  "Minimum value of the 'adjusted exponent' for any subnormal number,
 i.e. the minimum exponent of a subnormal number when it is in exponencial notation.
 This is the constant Etiny of the specification.")

(deftype unsigned-word ()
  `(unsigned-byte ,+decimal-slot-bits+))

(deftype decimal-slot ()
  `(mod ,(expt 10 +decimal-slot-digits+)))

(defstruct (decimal-float
	     (:conc-name df-)
	     (:constructor %make-df (slots exponent))
	     (:copier copy-df))
  (extra 0 :type #.`(unsigned-byte ,(+ 4 (* 2 +num-of-digits-bits+))))
  (slots 1 :type (or null (simple-array decimal-slot) condition))
  (exponent 0 :type (mod #.+internal-e-max+)))

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

  ;; About fsld and lsfd:
  ;; We store the slots (one slot is digit in base 10 ^ +decimal-slot-digits+)
  ;; in little-endian order, and, therefore, the last slot is the most significant one.

  ;; The first digit (in base 10) of the last slot is the position (counting upfrom 0 from the left)
  ;; of the most significant valid digit of that slot
  ;; (and, therefore, the most significant digit of the number).
  ;; The digits of the last slot which are more significant than fsld are unused and meant to be always zero.

  ;; Likewise, the last digit of the first slot is the position (also from the left counting from 0)
  ;; of the least significant valid digit of the first slot
  ;; (and, therefore, the least significant digit of the number).
  ;; The digits of the first slot which are more significant than lsfd are also unused and meant to be always zero.
  (def-field df-first-slot-last-digit 4 (+ 4 +num-of-digits-bits+))
  (def-field df-last-slot-first-digit  (+ 4 +num-of-digits-bits+)
	     (+ 4 (* 2 +num-of-digits-bits+))))

(defun df-zero-or-subnormal-p (x)
  (let ((x-slots (df-slots x)))
    (eql 0 (aref x-slots (1- (length x-slots))))))

(defun df-finite-p (x)
  (zerop (ldb (byte 2 1) (df-extra x))))

(defun df-normal-p (x)
  (zerop (ldb (byte 3 1) (df-extra x))))

(defun %logb (x)
  (+ (* +decimal-slot-digits+ (df-exponent x))
     (df-last-slot-first-digit x)))

(defun %exponent (x)
  (+ (* +decimal-slot-digits+ (+ (df-exponent x)
                                 (- 1 (length (df-slots x)))))
     (df-first-slot-last-digit x)))

(defun %count-digits (x)
  (+ (*  +decimal-slot-digits+ (- (length (df-slots x)) 1))
     (1+ (df-last-slot-first-digit x))
     (-  (df-first-slot-last-digit x))))

(defun make-digits-array (slots)
  (make-array slots :element-type 'decimal-slot
	      :initial-element 0))

(define-constant +expt-10+
    ;; Powers of 10 for fast reference.
    (let ((array (make-array +decimal-slot-digits+
			     :element-type 'decimal-slot)))
      (loop for i below +decimal-slot-digits+
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
  (check-type digits (mod #.+maximum-precision+))
  (let* ((adj-exponent (+ printed-exp (- dot-position 1)))
         (subnormal-digits (if (< adj-exponent +minimum-exponent+)
                               (- +minimum-exponent+ adj-exponent)
                               0))
         (digits (+ digits subnormal-digits)))
    (check-type adj-exponent (integer #.+tiny-exponent+ #.+maximum-exponent+))
    (multiple-value-bind (df-exponent lsfd) (floor adj-exponent +decimal-slot-digits+)
      (multiple-value-bind (slots-1 fsld) (ceiling (- digits lsfd 1) +decimal-slot-digits+)
        (values (make-digits-array (1+ slots-1)) (- fsld) lsfd df-exponent subnormal-digits)))))

(defun parse-info (x)
  ;; Abstracting away differences between normal and subnormal numbers information
  (let ((digits (%count-digits x))
        (exponent (%exponent x))
        (adj-exponent (%logb x))
        (x-slots (df-slots x))
        (fsld (df-first-slot-last-digit x))
        (lsfd (df-last-slot-first-digit x)))
    (when (df-subnormal-p x)
      (let* ((last-position (or (position-if #'plusp x-slots) 0))
             (discarded-slots (- (length x-slots) (1+ last-position)))
             (new-lsfd (first-digit-of-slot (aref x-slots last-position)))
             (discarded-digits (- (+ (* discarded-slots +decimal-slot-digits+)
                                     lsfd)
                                  new-lsfd)))
        (decf digits discarded-digits)
        (decf adj-exponent discarded-digits)
        (setf x-slots (subseq x-slots 0 (1+ last-position))
              lsfd new-lsfd)))
    (values digits exponent adj-exponent x-slots fsld lsfd)))
