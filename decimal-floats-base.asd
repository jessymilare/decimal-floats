;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2013 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :decimal-floats-system
  (:use :cl :asdf))

(in-package :decimal-floats-system)

(defsystem :decimal-floats-base
  :name "Decimal Floats base library"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Foreign function interface for decNumber library."
  :components ((:module "cffi"
                :components ((:file "package")
                             (:file "library" :depends-on ("package"))
                             (:file "dec-number-cffi" :depends-on ("library"))
                             (:file "dec-number-cffi-2" :depends-on ("dec-number-cffi")))))
  :depends-on (:alexandria :cffi :trivial-features :external-program))
