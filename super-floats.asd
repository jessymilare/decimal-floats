;;; -*- mode: lisp; indent-tabs: nil -*-

(defsystem :super-floats
  :serial t
  ;; add new files to this list:
  :components ((:file "package") (:file "super-floats"))
  :depends-on (#+nil :cl-ppcre))
