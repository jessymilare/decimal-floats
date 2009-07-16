;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :decimal-floats
  (:use :cl :alexandria)
  (:export
   ;; Constants
   #:+maximum-exponent+ #:+minimum-exponent+ #:+maximum-precision+ 
   ;; Structure
   #:decimal-float

   ;; Conditions
   #:decimal-float-condition #:decimal-operation-name
   #:decimal-operation-defined-result #:decimal-operation-arguments

   ;; Printing
   #:get-decimal-printing-format #:print-decimal #:parse-decimal-float))
