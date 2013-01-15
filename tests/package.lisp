;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2013 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :decimal-floats-tests
  (:use :cl :alexandria :lift :yacc :decimal-floats)
  (:export #:load-all-tests #:run-all-tests))
