;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (type fixnum *condition-flags* *condition-trap-enablers*))

(define-constant +all-conditions+
    '(decimal-conversion-syntax decimal-division-undefined decimal-division-impossible
      decimal-invalid-operation decimal-division-by-zero decimal-underflow
      decimal-subnormal decimal-overflow decimal-inexact decimal-rounded decimal-clamped)
  :test #'equal)

(defvar *decimal-local-error*)

(define-condition decimal-float-condition ()
  ((defined-result :accessor operation-defined-result :initarg :defined-result)
   (arguments :accessor operation-arguments :initarg :arguments)
   (operation :accessor operation-name :initarg :operation)))

(defun find-condition-flags (flag-list)
  "Transforms the list of symbols FLAG-LIST into an internal format suitable for
*CONDITION-FLAGS*. The list should contain only the following symbols, which are
names of conditions:

DECIMAL-CLAMPED  DECIMAL-DIVISION-BY-ZERO  DECIMAL-INEXACT
DECIMAL-INVALID-OPERATION  DECIMAL-OVERFLOW  DECIMAL-ROUNDED  DECIMAL-SUBNORMAL
DECIMAL-UNDERFLOW.

See these conditions' documentation for more details."
  (let ((flag-integer 0))
    (declare (fixnum flag-integer)
             (list flag-list))
    (when flag-list
      (loop for condition in +all-conditions+
         for i from 0
         if (member condition flag-list)
         do (setf (logbitp i flag-integer) t)))
    flag-integer))

(def-var-get-and-with (condition-flags nil)
  "Holds which conditions has been signalled during arithmetic operations.
Use GET-CONDITION-FLAGS to fetch them."
  (declare (fixnum condition-flags))
  (loop for condition in +all-conditions+
     for i from 0
     if (logbitp i (the fixnum condition-flags))
     collect condition))

(declaim (inline find-condition-trap-enablers get-condition-trap-enablers))

(defun find-condition-trap-enablers (flag-list)
  "Transforms the list of symbols FLAG-LIST into an internal format suitable for
*CONDITION-TRAP-ENABLERS*. The list should contain only the following symbols,
which are names of conditions:

DECIMAL-CLAMPED  DECIMAL-DIVISION-BY-ZERO  DECIMAL-INEXACT
DECIMAL-INVALID-OPERATION  DECIMAL-OVERFLOW  DECIMAL-ROUNDED  DECIMAL-SUBNORMAL
DECIMAL-UNDERFLOW.

If the returned value is bound to the variable *CONDITION-TRAP-ENABLERS*, then each
condition in the SIGNALLER-LIST will throw an error when found during internal
arithmetics."
  (find-condition-flags flag-list))

(def-var-get-and-with (condition-trap-enablers '(decimal-clamped decimal-division-by-zero
                                                 decimal-invalid-operation
                                                 decimal-overflow decimal-underflow))
  "Holds information about which conditions should be signalled with the
function ERROR when found. Use FIND-CONDITION-TRAP-ENABLERS to encode a list of
symbols into a suitable format for this variable."
  (get-condition-flags condition-trap-enablers))

(defmacro with-condition-flags* ((flags) &body body)
  "Like WITH-CONDITION-FLAGS, but returns two values: the value returned by BODY
as an implicit progn, and the list of (arithmetic) conditions that were
signalled during its execution."
  `(let ((*condition-flags* (find-condition-flags ,flags)))
     (values
      (progn ,@body)
      (get-condition-flags *condition-flags*))))

(defmacro with-operation ((operation-name condition-var &rest operation-arguments)
                          (&rest condition-case)
                          &body body)
  (with-gensyms (local-error condition-name new-default-result value)
    `(flet ((,local-error (,condition-name ,new-default-result)
              (values
               (let ((,condition-var
                      (make-condition ,condition-name :operation ',operation-name
                                      :arguments (list ,@operation-arguments))))
                 (setf (operation-defined-result ,condition-var)
                       (or ,new-default-result
                           (case ,condition-name
                             ,@condition-case)))
                 ,condition-var)
               #'(lambda (,value)
                   (return-from ,operation-name ,value)))))
       (let ((*decimal-local-error* #',local-error))
         ,@body))))

(defun get-condition-mask (condition)
  (ash 1 (position condition +all-conditions+)))

(defmacro decimal-error-cond ((defined-result &key return-p) &body conditions)
  (check-type return-p boolean)
  (with-gensyms (condition-var return-function condition-name bit-mask trap-mask)
    (let* ((conditions (mapcar #'ensure-list conditions))
           (masks (mapcar (compose #'get-condition-mask #'lastcar) conditions)))
      (once-only (defined-result)
        `(let* ((,bit-mask (logior ,@(loop for condition-spec in conditions
                                        for tests = (butlast condition-spec)
                                        for mask in masks
                                        collect (if tests
                                                    `(if (and ,@tests)
                                                         ,mask
                                                         0)
                                                    mask))))
                (,trap-mask (logand ,bit-mask *condition-trap-enablers*)))
           (setf *condition-flags* (logior *condition-flags* ,bit-mask))
           (if (plusp ,trap-mask)
               (let ((,condition-name (first (get-condition-flags ,bit-mask))))
                 (multiple-value-call #'signal-decimal-condition
                   (funcall *decimal-local-error* ,condition-name ,defined-result)
                   :return-p ,return-p))
               (locally ; avoid compiler-warnings of "deleting unreachable code"
                   #+sbcl (declare (optimize sb-ext:inhibit-warnings))
                   (or ,defined-result
                       (let ((,condition-name (first (get-condition-flags ,bit-mask))))
                         ,(if return-p
                              `(multiple-value-bind (,condition-var ,return-function)
                                   (funcall *decimal-local-error* ,condition-name nil)
                                 (funcall ,return-function
                                          (operation-defined-result ,condition-var)))
                              `(operation-defined-result
                                (funcall *decimal-local-error*
                                         ,condition-name nil))))))))))))

(macrolet ((def (name (&rest parents) documentation
                      &optional (format-string documentation)
                      &rest format-vars)
             (assert (find name +all-conditions+))
             `(define-condition ,name ,parents
                ()
                (:documentation ,documentation)
                (:report (lambda (condition stream)
                           (format stream
                                   ,(concatenate 'string format-string
                                                 "~%Operation: ~S; Arguments: ~S")
                                   ,@format-vars
                                   (operation-name condition)
                                   (operation-arguments condition)))))))

  (def decimal-clamped (decimal-float-condition)
    "The exponent of the result has been constrained or altered due to internal
representation limits.")

  (def decimal-division-by-zero (decimal-float-condition)
    "Attempt to divide finite number by zero or to calculate a negative power of
zero.")

  (def decimal-invalid-operation (decimal-float-condition)
    "Invalid operation.")

  (def decimal-rounded (decimal-float-condition)
    "The result was rounded due to the current precision.")

  (def decimal-inexact (decimal-rounded)
    "The result needed to be rounded due to the current precision and some
discarded digits were non-zero.")

  (def decimal-overflow (decimal-inexact)
    "The value of the adjusted exponent (as returned by decimal-logb) of the
result is greater than +maximum-exponent+ and cannot be returned.")

  (def decimal-subnormal (decimal-float-condition)
    "The result or the operation is subnormal, i.e., it has an adjusted exponent
 (as returned by decimal-logb) less than +minimum-exponent+.")

  (def decimal-underflow (decimal-subnormal decimal-inexact)
    "The result of the operation is subnormal and inexact, i.e., it has an
adjusted exponent (as returned by decimal-logb) less than +minimum-exponent+,
the result needed to be rounded and some discarded digits were non-zero.")

  (def decimal-conversion-syntax (decimal-invalid-operation)
    "The string given does not conform the numeric string syntax for decimal
numbers.")

  (def decimal-division-impossible (decimal-invalid-operation)
    "Integer division is impossible due to the current precision.")

  (def decimal-division-undefined (decimal-invalid-operation)
    "Attempt to divide zero by zero.")

  ;; insufficient-storage, invalid-context
  )

(defun signal-decimal-condition (condition return-function &key return-p)
  (let* ((defined-result (operation-defined-result condition))
         (value
          (restart-case (error condition)
            (return-defined-result ()
              :report (lambda (stream)
                        (let ((result defined-result))
                          (if (eq result 'to-be-calculated)
                              (format stream "Continue calculation.")
                              (format stream "Return ~A." result))))
              (operation-defined-result condition))
            (return-another-value (value)
              :report "Return another value."
              :interactive (lambda ()
                             (list (read-from-string
                                    (prompt t "Enter the value to be returned (it will be read):~%")
                                    :trim-spaces t)))
              value))))
    (if (or return-p
            (and (eq defined-result 'to-be-calculated)
                 (not (eq value 'to-be-calculated))))
        (funcall return-function value)
        value)))
