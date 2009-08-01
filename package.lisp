;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :decimal-floats
  (:use :cl :alexandria)
  (:export
   ;; Constants
   #:+maximum-exponent+ #:+minimum-exponent+ #:+maximum-precision+ 
   ;; Structure
   #:decimal-float

   ;; Conditions
   #:decimal-float-condition
   #:decimal-clamped #:decimal-division-by-zero #:decimal-inexact #:decimal-invalid-operation
   #:decimal-overflow #:decimal-rounded #:decimal-subnormal #:decimal-underflow #:decimal-conversion-syntax
   #:decimal-division-impossible #:decimal-division-undefined

   #:operation-name #:operation-defined-result #:operation-arguments
   #:return-defined-result #:return-another-value

   #:*condition-flags* #:*condition-signallers*
   #:get-condition-flags #:get-condition-signallers #:find-condition-flags #:find-condition-signallers
   #:with-condition-flags #:with-condition-flags* #:with-condition-signallers

   ;; Rounding
   #:*precision*
   #:*rounding-mode* #:get-rounding-mode #:find-rounding-mode #:with-rounding-mode

   ;; Printing
   #:get-decimal-printing-format #:find-decimal-printing-format #:*printing-format*
   #:print-decimal #:parse-decimal))
