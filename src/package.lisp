;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :decimal-floats
  (:use :cl :alexandria)
  (:export
   ;; Constants
   #:+maximum-exponent+ #:+minimum-exponent+ #:+maximum-precision+ #:+minimum-precision+
   ;; Structure
   #:decimal-float #:exponent #:adjusted-exponent

   ;; Conditions
   #:+all-conditions+
   #:decimal-float-condition
   #:decimal-clamped #:decimal-division-by-zero #:decimal-inexact
   #:decimal-invalid-operation
   #:decimal-overflow #:decimal-rounded #:decimal-subnormal #:decimal-underflow
   #:decimal-conversion-syntax
   #:decimal-division-impossible #:decimal-division-undefined

   #:operation-name #:operation-defined-result #:operation-arguments
   #:return-defined-result #:return-another-value

   #:*condition-flags* #:*condition-trap-enablers*
   #:get-condition-flags #:get-condition-trap-enablers #:find-condition-flags
   #:find-condition-trap-enablers
   #:with-condition-flags #:with-condition-flags* #:with-condition-trap-enablers

   ;; Rounding
   #:*precision* #:precision
   #:*rounding-mode*

   ;; Printing
   #:*printing-format*
   #:print-decimal #:decimal-to-string #:parse-decimal))
