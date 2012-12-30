;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :dec-number-cffi)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +decdpun+ 3)

  (defparameter *dec-number-bin-dir*
    (asdf:system-relative-pathname :decimal-floats "bin/"))

  (defparameter *dec-number-make-dir*
    (asdf:system-relative-pathname :decimal-floats "cffi/"))

  (defparameter *dec-number-make-environment*
    `(("DECLITEND" . #+little-endian 1 #+big-endian 0)
      #+nil ("DECDPUN" . ,+decdpun+)
      ("DECNUMBER_BIN_DIR" . ,(namestring *dec-number-bin-dir*))
      ("DECSYSTEM" . #+darwin "darwin" #+linux "linux"
                   #+windows "mingw" #+bsd "fbsd")))

  (define-foreign-library
      (dec-number-library :search-path *dec-number-bin-dir*)
    (:darwin "libdecnumber.dylib")
    (:unix "libdecnumber.so")
    (:windows "decNumber.dll"))

  (defun run-make-dec-number (option)
    (multiple-value-bind (status code)
        (external-program:run
         "make" `("-C" ,(namestring *dec-number-make-dir*) ,option)
         :environment *dec-number-make-environment*)
      (and (eq :exited status) (= 0 code))))

  (defun load-dec-number-library ()
    (handler-case (load-foreign-library 'dec-number-library)
      (load-foreign-library-error ()
        (run-make-dec-number "clean")
        (assert (and (run-make-dec-number "all")
                     (run-make-dec-number "install")))
        (load-foreign-library 'dec-number-library))))
  
  (load-dec-number-library))
 