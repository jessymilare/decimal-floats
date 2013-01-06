;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(defpackage :decimal-floats-system
  (:use :cl :asdf))

(in-package :decimal-floats-system)

(defsystem :decimal-floats
  :name "Decimal Floats"
  ; :version "0.0.1"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Implements decimal arithmetics as specified by standard
 http://speleotrove.com/decimal/"
  :components ((:module "src"
                :components ((:file "package")
                             (:file "structure" :depends-on ("package"))
                             (:file "utils" :depends-on ("structure"))
                             (:file "conditions" :depends-on ("utils"))
                             (:file "rounding" :depends-on ("utils" "conditions"))
                             (:file "printing" :depends-on
                                    ("utils" "structure" "conditions" "rounding"))
                             (:file "basic-operations" :depends-on
                                    ("structure" "utils"))
                             (:file "arithmetic" :depends-on
                                    ("structure" "utils" "conditions"))
                             (:file "conversions" :depends-on
                                    ("structure" "utils" "conditions")))))
  :depends-on (:alexandria))

(defmethod perform ((op test-op) (system (eql (find-system :decimal-floats))))
  (oos 'load-op :decimal-floats-tests)
  (oos 'test-op :decimal-floats-tests))
