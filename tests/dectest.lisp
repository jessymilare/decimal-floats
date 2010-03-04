;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats-tests)

(defun get-line-element (string state position)
  (let* ((first-char (char string position))
         (quote (find first-char '(#\' #\")))
         (length (length string)))
    (if (and quote (member state '(:operand :result)))
        (do* ((1+previous-end nil (1+ end))
              (end (position quote string :start (1+ position))
                   (position quote string :start (1+ 1+previous-end)))
              (sub (subseq string (1+ position) end)
                   (concatenate 'string sub
                                (subseq string 1+previous-end end))))
            ((not (and (< (1+ end) length)
                       (char= quote (char string (1+ end)))))
             (values sub (1+ end))))
        (let* ((end (or (position #\Space string :start position)
                         length))
               (sub (subseq string position end)))
          (values sub end)))))

(defun make-line-lexer (string)
  (let* ((length (length string))
         (state :first)
         (position 0))
    (named-lambda line-parser ()
      (flet ((get-value ()
               (multiple-value-bind (sub end) (get-line-element string state position)
                 (setf position end)
                 (ecase state
                   (:first
                    (if (char= (char string (1- end)) #\:)
                        (progn
                          (setf state :result)
                          (values 'directive (read-from-string sub t nil :end (1- (length sub)))))
                        (progn
                          (setf state :operation)
                          (values 'test-id (read-from-string sub)))))
                   (:condition
                    (values 'condition (symbolicate "DECIMAL-" (nstring-upcase (substitute #\- #\_ sub)))))
                   (:operand
                    (values 'operand sub))
                   (:result
                    (setf state :condition)
                    (values 'result sub))
                   (:operation
                    (setf state :operand)
                    (values 'operation (read-from-string sub)))))))
        (cond
          ((= position length)
           (values nil nil))
          ((eq (char string position) #\Space)
           (setf position (or (position #\Space string :start position :test (complement #'char=))
                              length))
           (line-parser))
          ((eq (char string position) #\-)
           (case (char string (1+ position))
             (#\-
              (setf position length)
              (values nil nil))
             (#\>
              (incf position 2)
              (setf state :result)
              (values '-> '->))
             (t (get-value))))
          (t (get-value)))))))

(define-parser *testcase-line-parser*
  (:start-symbol line)
  (:terminals (test-id operation operand -> result condition directive))
  (:muffle-conflicts t)

  (line
   (test-token #'(lambda (test-token)
                   (cons 'test test-token)))
   (directive-token #'(lambda (directive-token)
                        (cons 'directive directive-token)))
   nil)

  (test-token
   (test-id operation operands -> result conditions #'list))

  (operands
   (operand #'list)
   (operand operands #'cons))

  (conditions
   nil
   (condition conditions #'cons))

  (directive-token
   (directive result #'list)))

(defun make-file-lexer (file)
  (make-stream-lexer (open file :element-type 'base-char :external-format :ascii)
                     :close-p t))

(defun make-stream-lexer (stream &key (close-p nil))
  (let* ((state :toplevel)
         (function
          (named-lambda stream-lexer ()
            (flet ((new-line ()
                     (handler-case (read-line stream nil :eof)
                       (stream-error ()
                         :eof))))
              (let ((line (new-line)))
                (when (eq line :eof)
                  (when close-p
                    (close stream))
                  (return-from stream-lexer
                    (values nil nil)))
                (setf line (delete #\Return line))
                (destructuring-bind (kind &rest form)
                    (or (parse-with-lexer (make-line-lexer line) *testcase-line-parser*)
                        '(nil))
                  (if kind
                      (ecase kind
                        (directive
                         (setf state :toplevel)
                         (values kind form))
                        (test              
                         (let ((terminal
                                (ecase state
                                  (:toplevel (setf state :test)
                                             'new-test)
                                  (:test 'test-form))))
                           (values terminal form))))
                      (progn
                        (setf state :toplevel)
                        (stream-lexer)))))))))
    (when close-p
      (tg:finalize function (curry #'close stream)))
    function))

(defun get-operation (operation)
  (ecase operation
    ((tosci toeng apply) #'identity)))

(defmacro apply-getting-conditions (function operands)
  (with-gensyms (conditions c)
    `(let ((,conditions nil))
       (values
        (handler-bind ((decimal-float-condition
                        (lambda (,c)
                          (push (class-name (class-of ,c)) ,conditions)
                          (invoke-restart 'return-defined-result))))
          (apply ,function ,operands))
        (nreverse ,conditions)))))

(defvar *testcase-precision* *precision*)
(defvar *testcase-rounding-mode* *rounding-mode*)
(defvar *testcase-maxexponent* +maximum-exponent+)
(defvar *testcase-minexponent* +minimum-exponent+)
(defvar *testcase-version* nil)
(defvar *testcase-filename* nil)
(defvar *testcase-testsuite* 'decimal-floats-testsuite)
(defvar *testcase-parent-testsuite* nil)
(defvar *testcase-extended* 1)
(defvar *testcase-clamp* 0)

(defun convert-testcase-file (input &key (eval-p nil))
  (let* ((input (pathname input))
         (*testcase-precision* *precision*)
         (*testcase-rounding-mode* :round-half-up)
         (*testcase-maxexponent* +maximum-exponent+)
         (*testcase-minexponent* +minimum-exponent+)
         (*testcase-version* nil)
         (*testcase-filename* input)
         (*testcase-parent-testsuite* *testcase-testsuite*)
         (*testcase-testsuite* (symbolicate (string-upcase (pathname-name input))
                                            "-TESTSUITE"))
         (*testcase-extended* 1)
         (*testcase-clamp* 0))
    (format t ";;; Processing of file ~S has been started.~%" input)
    (let ((parsed-output (parse-with-lexer (make-file-lexer input)
                                           *testcase-file-parser*)))
      (format t ";;; Processing of file ~S has been finished.~%" input)
      (when eval-p
        (format t ";;; Starting evaluation...~%")
        (handler-case (eval parsed-output)
          (error (c)
                 (cerror "Continue without evaluating" c)
                 (format t ";;; Error during evaluation, ignored.~%")))
        (format t ";;; Evaluation finished.~%"))
      parsed-output)))

(define-parser *testcase-file-parser*
  (:start-symbol all-forms)
  (:terminals (directive new-test test-form))
  (:muffle-conflicts t)

  (all-forms
   (toplevel-forms
    #'(lambda (toplevel-forms)
        (list* 'progn
               '(in-package :decimal-floats-tests)
               `(deftestsuite ,*testcase-testsuite* (,*testcase-parent-testsuite*)
                  ())
               (delete nil toplevel-forms)))))
    
  (toplevel-forms
   nil
   (toplevel-form toplevel-forms #'cons))
    
  (toplevel-form
   (directive #'(lambda (args)
                  (apply #'change-directive args)
                  nil))
   (new-test test-forms #'(lambda (first-test other-tests)
                           ;; Argh! Ugly workaround.
                           (create-test first-test other-tests))))

  (test-forms
   nil
   (test-form test-forms #'cons)))

(defun change-directive (directive value)
  (case directive
    (precision (setf *testcase-precision* (parse-integer value)))
    (rounding (setf *testcase-rounding-mode*
                    (intern (concatenate 'string
                                         "ROUND-"
                                         (nstring-upcase (substitute #\- #\_ value)))
                            :keyword)))
    (maxexponent (setf *testcase-maxexponent* (parse-integer value)))
    (minexponent (setf *testcase-minexponent* (parse-integer value)))
    (version
     (if *testcase-version*
         (warn "New occurrence of version directive found, using the first one:~%~
version being used: ~S, ignored version: ~S."
               *testcase-version* value)
         (setf *testcase-version* value)))
    (extended (setf *testcase-extended* (parse-integer value)))
    (clamp (setf *testcase-clamp* (parse-integer value)))
    (dectest
     (format t ";;; Directive dectest encountered. Processing file ~S." value)
     (convert-testcase-file (make-pathname :name value :defaults *testcase-filename*)))
    (t (warn "Unknown directive found during testcase parsing: ~S." directive))))

(defun create-test (first-test other-tests)
  (macrolet ((with-directive-tests ((form skipping-what) &body all-tests)
               (with-gensyms (directives)
                 (once-only (form skipping-what)
                   `(let (,directives)
                      ,@(mapcar (curry #'apply
                                       (lambda (test directive)
                                         `(when ,test
                                            (push (list ',directive
                                                        ,(symbolicate "*TESTCASE-" directive "*"))
                                                  ,directives))))
                                all-tests)
                      (if ,directives
                          (warn "Skipping ~S due to the ~
~[~;value of this~:;values of these~] directive~:p: ~
~%~{~{~S: ~S~}~^; ~}.~%Skipped form:~%~S"
                                ,skipping-what (length ,directives)
                                ,directives ,form)
                          ,form))))))
    (let* ((first-test-name (car first-test))
           (last-test-name (and other-tests (caar (last other-tests))))
           (ctest-name (symbolicate *testcase-testsuite* "-"
                                    (string-upcase first-test-name)
                                    (if last-test-name "-TO-" "")
                                    (string-upcase (or last-test-name ""))))
           (inner-forms
            (delete nil
                    (mapcar
                     (curry #'apply
                            (lambda (test-id operation operands -> result conditions)
                              (declare (ignore ->))
                              ;; FIXME: no maxexponent or minexponent
                              ;; in the standard testcase files are supported by this implementation,
                              ;; so, for now, there is no way to simulate overflow or underflow tests.
                              (with-directive-tests
                                  (`(test-comparison ',test-id ',operation
                                                     ',operands ',result ',conditions)
                                    "test")
                                ((and (/= +maximum-exponent+ *testcase-maxexponent*)
                                      (intersection '(decimal-overflow clamped) conditions))
                                 maxexponent)
                                ((and (/= +minimum-exponent+ *testcase-minexponent*)
                                      (intersection '(decimal-subnormal decimal-underflow clamped)
                                                    conditions))
                                 minexponent))))
                     (cons first-test other-tests))))
           (ctest-form
            (when inner-forms
              `(addtest (,*testcase-testsuite*)
                 ,ctest-name
                 (let ((*precision* ,*testcase-precision*)
                       (*rounding-mode* (find-rounding-mode ,*testcase-rounding-mode*)))
                   ,@inner-forms)))))
      (with-directive-tests (ctest-form "tests")
        ((/= 1 *testcase-extended*) extended)
        ((/= 0 *testcase-clamp*) clamp)
        ((> *testcase-maxexponent* +maximum-exponent+) maxexponent)
        ((< *testcase-minexponent* +minimum-exponent+) minexponent)
        ((> *testcase-precision*  +maximum-precision+) precision)))))

(deftestsuite decimal-floats-testsuite ()
  ())

(defun test-comparison (test-id operation operands result conditions)
  (multiple-value-bind (new-result new-conditions)
      (apply-getting-conditions
       (get-operation operation)
       (mapcar (rcurry #'parse-decimal :round-p (member operation '(tosci toeng apply)))
               operands))
    (ensure-same
     (decimal-to-string new-result
                        :format (case operation
                                  (tosci :scientific)
                                  (toeng :engineering)
                                  (t :scientific)))
     result
     :test #'equal
     :ignore-multiple-values? t
     :report (format nil "Returned value mismatch in test ~S."
                     test-id))
    (ensure-same
     new-conditions conditions
     :test #'equal
     :ignore-multiple-values? t
     :report (format nil "Signalled conditions mismatch in test ~S."
                     test-id))
    new-result))