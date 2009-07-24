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
   #:$clamped #:$division-by-zero #:$inexact #:$invalid-operation
   #:$overflow #:$rounded #:$subnormal #:$underflow #:$conversion-syntax
   #:$division-impossible #:$division-undefined

   #:operation-name #:operation-defined-result #:operation-arguments
   #:return-defined-result #:return-another-value

   ;; Printing
   #:get-decimal-printing-format #:print-decimal #:parse-decimal-float))
