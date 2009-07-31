;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (type fixnum *condition-flags* *condition-signallers*))

(define-constant +all-conditions+
    '(decimal-clamped decimal-division-by-zero decimal-inexact decimal-invalid-operation
      decimal-overflow decimal-rounded decimal-subnormal decimal-underflow
      ;; decimal-conversion-syntax decimal-division-impossible decimal-division-undefined
      )
  :test #'equal)

(defvar *decimal-local-error*)

(declaim (inline decimal-float-condition))

(define-condition decimal-float-condition ()
  ((defined-result :accessor operation-defined-result :initarg :defined-result)
   (arguments :accessor operation-arguments :initarg :arguments)
   (operation :accessor operation-name :initarg :operation)))

(defgeneric get-condition-bit (condition))

(defun find-condition-flags (flag-list)
  "Transforms the list of symbols FLAG-LIST into an internal format suitable for *CONDITION-FLAGS*.
 The list should contain only the following symbols, which are names of conditions:

 DECIMAL-CLAMPED  DECIMAL-DIVISION-BY-ZERO  DECIMAL-INEXACT  DECIMAL-INVALID-OPERATION
 DECIMAL-OVERFLOW  DECIMAL-ROUNDED  DECIMAL-SUBNORMAL  DECIMAL-UNDERFLOW.

 See some these conditions' documentation for more details."
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
 Use GET-CONDITION-FLAGS to fetch them.
 This variable is useful to see what is \"going wrong\" with the arithmetics,
 but has no internal effect."
  (declare (fixnum condition-flags))
  (loop for condition in +all-conditions+
     for i from 0
     if (logbitp i (the fixnum condition-flags))
     collect condition))

(declaim (inline find-condition-signallers get-condition-signallers))

(defun find-condition-signallers (signaller-list)
  "Transforms the list of symbols FLAG-LIST into an internal format suitable for *CONDITION-SIGNALLERS*.
 The list should contain only the following symbols, which are names of conditions:

 DECIMAL-CLAMPED  DECIMAL-DIVISION-BY-ZERO  DECIMAL-INEXACT  DECIMAL-INVALID-OPERATION
 DECIMAL-OVERFLOW  DECIMAL-ROUNDED  DECIMAL-SUBNORMAL  DECIMAL-UNDERFLOW.

 If the returned value is bound to the variable *CONDITION-SIGNALLERS*, then each condition
 in the SIGNALLER-LIST will throw an error when found during internal arithmetics."
  (find-condition-flags signaller-list))

(def-var-get-and-with (condition-signallers '(decimal-clamped decimal-division-by-zero decimal-invalid-operation
                                              decimal-overflow decimal-underflow))
  "Holds information about which conditions should be signalled with the function ERROR
 when found. Use FIND-CONDITION-SIGNALLERS to encode a list of symbols into a suitable
 format for this variable."
  (get-condition-flags condition-signallers))

(defmacro with-condition-flags* ((&rest flags) &body body)
  "Like WITH-CONDITION-FLAGS, but returns two values: the value returned by BODY 
 as an implicit progn, and the list of (arithmetic) conditions that were signalled during
 its execution."
  `(let ((*condition-flags* (get-condition-flags ',flags)))
     (values
      (progn ,@body)
      (parse-condition-flags *condition-flags*))))

(defmacro with-operation ((operation-name condition-var &rest operation-arguments)
                          (&rest condition-case)
                          &body body)
  (with-gensyms (local-error condition-name new-default-result)
    `(flet ((,local-error (,condition-name ,new-default-result)
              (signal-decimal-condition
               (let ((,condition-var
                      (make-condition ,condition-name :operation ',operation-name
                                      :arguments (list ,@operation-arguments))))
                 (setf (operation-defined-result ,condition-var)
                       (or ,new-default-result
                           (case ,condition-name
                             ,@condition-case)))
                 ,condition-var))))
       (let ((*decimal-local-error* #',local-error))
         ,@body))))

(defmacro decimal-error-cond (defined-result &body conditions)
  (with-gensyms (condition-signallers condition-flags condition-var local-error)
    (let* ((conditions (mapcar #'ensure-list conditions))
           (bit-numbers (mapcar (compose #'get-condition-bit #'car #'last) conditions)))
      (once-only (defined-result)
        `(let ((,local-error *decimal-local-error*)
               (,condition-signallers *condition-signallers*)
               (,condition-flags *condition-flags*))
           (unless (zerop ,condition-signallers)
             ,@(loop for condition-spec in conditions
                  for tests = (butlast condition-spec)
                  for condition = (car (last condition-spec))
                  for bit-number in bit-numbers
                  collect `(and (logbitp ,bit-number ,condition-signallers)
                                ,@tests
                                (funcall ,LOCAL-ERROR ',condition ,defined-result)))
             (setf *condition-flags* (logior ,condition-flags
                                             ,@(loop for bit-number in bit-numbers
                                                  collect `(if (and ,@tests)
                                                               (ash 1 ,bit-number)
                                                               0)))))
           (locally ;; avoid compiler-warnings of "deleting unreachable code"
               #+sbcl (declare (optimize sb-ext:inhibit-warnings))
               (or ,defined-result
                   (handler-case
                       (cond
                         ,@(loop for condition-spec in conditions
                              for tests = (butlast condition-spec)
                              for condition = (car (last condition-spec))
                              collect `((and ,@tests) (funcall ,LOCAL-ERROR ',condition nil))))
                     (decimal-float-condition (,condition-var)
                       (operation-defined-result ,condition-var))))))))))

(macrolet ((def (name (parent) documentation &optional (format-string documentation) &rest format-vars)
             (let ((bit-number (position name +all-conditions+)))
               `(progn
                  (define-condition ,name (,parent)
                    ()
                    (:documentation ,documentation)
                    (:report (lambda (condition stream)
                               (format stream ,(concatenate 'string format-string
                                                            "~%Operation: ~A; Arguments: ~A")
                                       ,@format-vars
                                       (operation-name condition) (operation-arguments condition)))))
                  (defmethod get-condition-bit ((condition (eql ',name)))
                    ,(or bit-number
                         `(get-condition-bit ',parent)))))))

  (def decimal-clamped (decimal-float-condition)
    "The exponent of the result has been constrained or altered due to internal representation limits.")

  (def decimal-division-by-zero (decimal-float-condition)
    "Attempt to divide finite number by zero or to calculate a negative power of zero.")

  (def decimal-inexact (decimal-float-condition)
    "The result needed to be rounded due to the current precision~
 and any discarded digits were non-zero.")

  (def decimal-invalid-operation (decimal-float-condition)
    "Invalid operation.")

  (def decimal-overflow (decimal-float-condition)
    "The value of the adjusted exponent (as returned by decimal-logb) of the result~
 is greater than +maximum-exponent+ and cannot be returned.")

  (def decimal-rounded (decimal-float-condition)
    "The result was rounded due to the current precision.")

  (def decimal-subnormal (decimal-float-condition)
    "The result or the operation is subnormal, i.e.,~
 it has an adjusted exponent (as returned by decimal-logb) less than +minimum-exponent+.")

  (def decimal-underflow (decimal-float-condition)
    "The result of the operation is subnormal and inexact, i.e.,~
 it has an adjusted exponent (as returned by decimal-logb) less than +minimum-exponent+,~
 the result needed to be rounded and any discarded digits were non-zero.")

  (def decimal-conversion-syntax (decimal-invalid-operation)
    "The string given does not conform the numeric string syntax for decimal numbers.")

  (def decimal-division-impossible (decimal-invalid-operation)
    "Integer division is impossible due to the current precision.")

  (def decimal-division-undefined (decimal-invalid-operation)
    "Attempt to divide zero by zero.")

  ;; insufficient-storage, invalid-context
  )

(defun signal-decimal-condition (condition)
  (restart-case (error condition)
    (return-defined-result ()
      :report (lambda (stream)
                (format stream "Return ~A" (operation-defined-result condition)))
      (operation-defined-result condition))
    (return-another-value (value)
      :report "Return another value"
      :interactive (lambda ()
                     (list (prompt t "Enter the value to be returned: ")))
      value)))
