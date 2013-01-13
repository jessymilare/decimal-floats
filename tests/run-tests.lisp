;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats-tests)

(defparameter *tests-directory*
  (asdf:system-relative-pathname :decimal-floats "tests/"))

(defparameter *files-to-test* '("abs" "base" "logb" "copy" "copyabs" "copynegate"
                                "copysign" "class" "max" "maxmag" "min" "minmag"
                                "minus" "plus" "samequantum" "scaleb"))

(defun download-file (url output)
  ;; Patches welcome ;)
  (format *trace-output* "Downloading file ~a~%" url)
  (external-program:run "wget" `(,url "-O" ,(namestring output))
                        :output *trace-output*)
  output)

(defun unzip-file (filename output-directory)
  (format *trace-output* "Unzipping file ~a~%" filename)
  (external-program:run "unzip" `(,(namestring filename) "-d"
                                   ,(namestring output-directory))
                        :output *trace-output*)
  output-directory)

(defun load-all-tests ()
  (let ((input-directory (merge-pathnames #P"dectest/" *tests-directory*))
        (output-directory (ensure-directories-exist
                           (merge-pathnames #P"output/" *tests-directory*))))
    (unless (cl-fad:directory-exists-p input-directory)
      (unzip-file
       (download-file "http://speleotrove.com/decimal/dectest.zip"
                      (merge-pathnames #P"dectest.zip" *tests-directory*))
       input-directory))
    (dolist (filename *files-to-test*)
      (load (nth-value 1 (convert-testcase-file
                          (make-pathname :name filename :type "decTest"
                                         :defaults input-directory)
                          :output-directory output-directory))))))

(defun run-all-tests ()
  (let ((report-pathname (make-pathname :name "tests-output"
                                        :defaults *tests-directory*)))
    (ignore-errors (delete-file report-pathname))
    (let ((result (run-tests :suite 'decimal-floats-testsuite
                             :report-pathname report-pathname)))
      (describe-test-result result *standard-output*)
      result)))
