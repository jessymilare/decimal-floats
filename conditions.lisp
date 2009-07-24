;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (type fixnum *condition-flags* *condition-signallers*))

(define-constant +all-conditions+
    '($clamped $division-by-zero $inexact $invalid-operation
      $overflow $rounded $subnormal $underflow
      ;; $conversion-syntax $division-impossible $division-undefined
      )
  :test #'equal)

(declaim (inline decimal-float-condition))

(define-condition decimal-float-condition ()
  ((defined-result :accessor operation-defined-result :initarg :defined-result)
   (arguments :accessor operation-arguments :initarg :arguments)
   (operation :accessor operation-name :initarg :operation)))

(defgeneric get-condition-bit (condition))

(defun find-condition-flags (flag-list)
  "Transforms the list of symbols FLAG-LIST into an internal format suitable for *CONDITION-FLAGS*.
 The list should contain only the following symbols, which are names of conditions:

 $CLAMPED  $DIVISION-BY-ZERO  $INEXACT  $INVALID-OPERATION
 $OVERFLOW  $ROUNDED  $SUBNORMAL  $UNDERFLOW.

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

 $CLAMPED  $DIVISION-BY-ZERO  $INEXACT  $INVALID-OPERATION
 $OVERFLOW  $ROUNDED  $SUBNORMAL  $UNDERFLOW.

 If the returned value is bound to the variable *CONDITION-SIGNALLERS*, then each condition
 in the SIGNALLER-LIST will throw an error when found during internal arithmetics."
  (find-condition-flags signaller-list))

(def-var-get-and-with (condition-signallers '($clamped $division-by-zero $invalid-operation
                                              $overflow $underflow))
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
  (with-gensyms (condition-name new-default-result)
    `(flet ((LOCAL-ERROR (,condition-name ,new-default-result)
              (signal-decimal-condition
               (let ((,condition-var
                      (make-condition ,condition-name :operation ',operation-name
                                      :arguments (list ,@operation-arguments))))
                 (setf (operation-defined-result ,condition-var)
                       (or ,new-default-result
                           (case ,condition-name
                             ,@condition-case)))
                 ,condition-var))))
       ,@body)))

(defmacro calling-local-error (LOCAL-ERROR defined-result &rest conditions)
  (with-gensyms (condition-signallers condition-flags condition)
    (let ((bit-numbers (mapcar #'get-condition-bit conditions)))
      (once-only (LOCAL-ERROR defined-result)
        `(let ((,condition-signallers *condition-signallers*)
               (,condition-flags *condition-flags*))
           (unless (zerop ,condition-flags)
             (cond
               ,@(loop for condition in conditions
                    for bit-number in bit-numbers
                    collect `((logbitp ,bit-number ,condition-signallers)
                              (funcall ,LOCAL-ERROR ',condition ,defined-result)))
               (t (setf *condition-flags* (logior ,condition-flags
                                                  ,(reduce #'logior bit-numbers
                                                           :key (curry #'ash 1)))))))
           ,(if defined-result
                defined-result
                (progn
                  (assert (not (cdr conditions)))
                  `(handler-case
                       (funcall ,LOCAL-ERROR ',(first conditions) nil)
                     (decimal-float-condition (,condition)
                       (operation-defined-result ,condition))))))))))

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

  (def $clamped (decimal-float-condition)
    "The exponent of the result has been constrained or altered due to internal representation limits.")

  (def $division-by-zero (decimal-float-condition)
    "Attempt to divide finite number by zero or to calculate a negative power of zero.")

  (def $inexact (decimal-float-condition)
    "The result needed to be rounded due to the current precision~
 and any discarded digits were non-zero.")

  (def $invalid-operation (decimal-float-condition)
    "Invalid operation.")

  (def $overflow (decimal-float-condition)
    "The value of the adjusted exponent (as returned by $logb) of the result~
 is greater than +maximum-exponent+ and cannot be returned.")

  (def $rounded (decimal-float-condition)
    "The result was rounded due to the current precision.")

  (def $subnormal (decimal-float-condition)
    "The result or the operation is subnormal, i.e.,~
 it has an adjusted exponent (as returned by $logb) less than +minimum-exponent+.")

  (def $underflow (decimal-float-condition)
    "The result of the operation is subnormal and inexact, i.e.,~
 it has an adjusted exponent (as returned by $logb) less than +minimum-exponent+,~
 the result needed to be rounded and any discarded digits were non-zero.")

  (def $conversion-syntax ($invalid-operation)
    "The string given does not conform the numeric string syntax for decimal numbers.")

  (def $division-impossible ($invalid-operation)
    "Integer division is impossible due to the current precision.")

  (def $division-undefined ($invalid-operation)
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
