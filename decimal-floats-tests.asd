;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :decimal-floats-system
  (:use :cl :asdf))

(in-package :decimal-floats-system)

(defsystem :decimal-floats-tests
  :name "Decimal Floats"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Test system for decimal-floats"
  :components ((:module "tests"
                :components ((:file "package")
                             (:file "dectest" :depends-on ("package")))))
  :depends-on (:alexandria :lift :yacc :decimal-floats :trivial-garbage))
