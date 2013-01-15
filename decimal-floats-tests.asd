;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2013 Gustavo Henrique Milaré
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
                             (:file "dectest" :depends-on ("package"))
                             (:file "run-tests" :depends-on ("dectest")))))
  :depends-on (:alexandria :lift :yacc :decimal-floats :trivial-garbage
                           :external-program :cl-fad))

(defmethod perform :after ((op load-op) (system (eql (find-system :decimal-floats-tests))))
  (funcall (find-symbol "LOAD-ALL-TESTS" :decimal-floats-tests)))

(defmethod perform ((op test-op) (system (eql (find-system :decimal-floats-tests))))
  (funcall (find-symbol "RUN-ALL-TESTS" :decimal-floats-tests)))
