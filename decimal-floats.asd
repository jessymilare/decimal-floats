;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage :decimal-floats-system
  (:use :cl :asdf))

(in-package :decimal-floats-system)

(defsystem :decimal-floats
  :name "Decimal Floats"
  ; :version "0.0.1"
  :maintainer "Gustavo Henrique Milaré"
  :author "Gustavo Henrique Milaré"
  :licence "MIT style"
  :description "Implements decimal arithmetics"
  :components ((:file "package")
	       (:file "structure" :depends-on ("package"))
               (:file "utils" :depends-on ("structure"))
               (:file "printing" :depends-on ("utils" "structure")))
  :depends-on (:alexandria))
