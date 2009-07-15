;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :decimal-floats
  (:use :cl :alexandria)
  (:export
   #:+maximum-exponent+ #:+minimum-exponent+ #:+maximum-precision+ 
   #:decimal-float))
