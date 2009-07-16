;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :decimal-floats)

(define-condition decimal-float-condition ()
  ((defined-result :accessor operation-defined-result)
   (arguments :accessor operation-arguments :initarg :arguments)
   (operation :accessor operation-name :initarg :operation)))

(defmacro with-condition-signalling ((operation-name (condition-var) &rest local-error-specs)
				     &body body)
  `(restart-case
       (flet ((local-error (condition-name)
		(case condition-name
		  ,@(loop for (name (&rest all-keys) value-form)
		            in local-error-specs
		       collect (let ((,condition-var
				      `(make-condition ',name :operation ',operation-name
						       ,@all-keys)))
				 (setf (operation-defined-result ,condition-var)
				       ,value-form)
				 (signal-decimal-condition ,condition-var))))))
	 (progn ,@body))))
(macrolet ((def (name parents fields format-string &rest format-vars))
	   `(define-condition ,name ,parents ,fields
	      (:report (lambda (condition stream)
			 (format stream ,format-string ,@format-vars)))))
  (def invalid-operation (decimal-float-condition)
    ()
    "Invalid operation: ~A was called with arguments ~A."
    (operation-name condition) (operation-arguments condition))

  (def conversion-syntax (invalid-operation)
    ()
    "The string does not conform the numeric string syntax for decimal numbers: \"~A\"."
    (first (operation-arguments condition)))

  (def division-by-zero (decimal-float-condition)
    ()
    "Attempt to divide finite number by zero or to calculate a negative power of zero~
 in ~A called with arguments ~A."
    (operation-name condition) (operation-arguments condition))

  (def division-impossible (invalid-operation)
    ()
    "Integer division in ~A with arguments ~A is impossible due to the current precision."
    (operation-name condition) (operation-arguments condition))

  (def division-undefined (invalid-operation)
    ()
    "Attempt to divide zero by zero in ~A."
    (operation-name condition))

  (def rounded (decimal-float-condition)
    ()
    "The result of the operation ~A with arguments ~A was rounded due to the current precision."
    (operation-name condition) (operation-arguments condition))

  (def inexact (rounded)
    ()
    "The result of the operation ~A with arguments ~A needed to be rounded due to the current precision~
 and any discarded digits were non-zero."
    (operation-name condition) (operation-arguments condition))

  ;; insufficient-storage, invalid-context
  (def overflow (inexact)
    ()
    "The value of the adjusted exponent (as returned by $logb) of the result~
 of the operation ~A with arguments ~A is greater than +maximum-exponent+
 and cannot be returned."
    (operation-name condition) (operation-arguments condition))

  (def subnormal (decimal-float-condition)
    ()
    "The result of the operation ~A with arguments ~A is subnormal, i.e.,~
 it has an adjusted exponent (as returned by $logb) less than +minimum-exponent+."
    (operation-name condition) (operation-arguments condition))

  (def underflow (inexact subnormal)
    ()
    "The result of the operation ~A with arguments ~A is subnormal and inexact, i.e.,~
 it has an adjusted exponent (as returned by $logb) less than +minimum-exponent+,~
 the result needed to be rounded and any discarded digits were non-zero."
    (operation-name condition) (operation-arguments condition)))
