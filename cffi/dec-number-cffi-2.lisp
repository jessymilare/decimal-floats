;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :dec-number-cffi)

;;; Beware with dirty macrolets :)

(macrolet ((def (function &optional flags notp)
               `(progn (declaim (inline ,function)
                                (ftype (function (foreign-pointer) boolean)
                                       ,function))
                       (defun ,function (number)
                         (declare (ignorable number))
                         ,(if flags
                              `(,(if notp 'not 'identity)
                                 (logtest (foreign-slot-value number 'decnumber 'bits)
                                          (logior ,@flags)))
                              t)))))
  (def number-is-canonical)
  (def number-is-finite (+flag-special+) t)
  (def number-is-infinite (+flag-inf+))
  (def number-is-nan (+flag-qnan+ +flag-snan+))
  (def number-is-signed (+flag-neg+))
  (def number-is-qnan (+flag-qnan+))
  (def number-is-snan (+flag-snan+))
  (def number-is-special (+flag-special+)))

(declaim (inline number-is-zero number-is-negative
                 number-radix number-canonical)
         (ftype (function (foreign-pointer) boolean)
                number-is-zero number-is-negative)
         (ftype (function (foreign-pointer) (integer 10 10))
                number-radix)
         (ftype (function (foreign-pointer) foreign-pointer)))

(defun number-is-zero (number)
  (and (= 0 (foreign-slot-value number 'decnumber 'lsu))
       (= 1 (foreign-slot-value number 'decnumber 'digits))
       (not (logtest (foreign-slot-value number 'decnumber 'bits)
                     +flag-special+))))

(defun number-is-negative (number)
  (let ((bits (foreign-slot-value number 'decnumber 'bits)))
    (and (logtest bits +flag-neg+)
         (not (logtest bits (logior +flag-qnan+ +flag-snan+))))))

(defun number-canonical (number)
  number)

(macrolet ((rename (function args)
             (let ((%function (symbolicate '% function)))
               `(progn
                  (declaim (inline ,function))
                  (defun ,function ,args
                    (,%function ,@args)))))
           (renames (types suffixes args)
             `(progn
                ,@(loop for type in types nconcing
                       (loop for suffix in suffixes
                          for function = (symbolicate type '- suffix)
                          collect
                            `(rename ,function (,type ,@args))))
                ,@(unless (member 'single types)
                          (loop for suffix in suffixes
                             for function = (symbolicate 'single '- suffix)
                             for %function = (symbolicate '% 'double '- suffix)
                             collect
                               `(progn
                                  (declaim (inline ,function))
                                  (defun ,function (single ,@args)
                                    (let ((double (single-to-double single nil)))
                                      (unwind-protect (,%function double ,@args)
                                        (foreign-free double)))))))))
           (defs (types suffix &body body)
               `(progn
                  ,@(loop for type in types
                       for function = (symbolicate type '- suffix)
                       collect
                         `(macrolet
                              ((call (suffix)
                                 (let ((%function (symbolicate '% ',type '- suffix)))
                                   `(locally (declare (inline ,%function))
                                      (,%function ,',type)))))
                            (declaim (inline ,function))
                            (defun ,function (,type)
                              ,@body)))
                  ,(unless (member 'single types)
                           (let ((function (symbolicate 'single '- suffix)))
                             `(macrolet
                                  ((call (suffix)
                                     (let ((%function (symbolicate '% 'double '- suffix)))
                                       `(locally (declare (inline ,%function))
                                          (,%function double)))))
                                (declaim (inline ,function))
                                (defun ,function (single)
                                  (let ((double (single-to-double single nil)))
                                    (unwind-protect (progn ,@body)
                                      (foreign-free double))))))))))

  (rename context-default (context type))
  
  (rename number-to-int32 (number context))
  (rename number-to-uint32 (number context))
  (rename number-class (number context))
  (rename number-class-to-string (class))
  (rename number-version ())

  (renames (double quad)
           (is-canonical is-finite is-infinite is-nan is-negative is-signed is-zero) ())
  (renames (double quad) (to-int32 to-uint32 to-int32-exact to-uint32-exact)
           (context rounding))
  (renames (double quad) (class class-string digits) ())
  (renames (single double quad) (get-exponent) ())
  (renames (single double quad) (set-exponent) (context exponent))

  (defs (double quad) is-qnan
    (and (call is-nan) (not (call is-signalling))))

  (defs (double quad) is-snan
    (call is-signalling))

  (defs (double quad) is-special
    (not (call is-finite))))

(defun mem-ref-uint (pointer bytes)
  ;; Suppresses various "deleting unreachable code" warnings
  ;; (see http://bugs.launchpad.net/sbcl/+bug/309115)
  (declare (optimize #+sbcl sb-ext:inhibit-warnings))
  (ecase bytes
    (16 #+little-endian (logior (mem-aref pointer :uint64 0)
                                (ash (mem-aref pointer :uint64 1) 64))
        #+big-endian (logior (ash (mem-aref pointer :uint64 0) 8)
                             (mem-aref pointer :uint64 1)))
    (8 (mem-ref pointer :uint64))
    (4 (mem-ref pointer :uint32))
    (2 (mem-ref pointer :uint16))
    (1 (mem-ref pointer  :uint8))))

(defun (setf mem-ref-uint) (value pointer bytes)
  (declare (optimize #+sbcl sb-ext:inhibit-warnings))
  (ecase bytes
    (16
     (check-type value (unsigned-byte 128))
     (let ((low  (ldb (byte 64 0)  value))
           (high (ldb (byte 64 64) value)))
       #+little-endian (setf (mem-aref pointer :uint64 0) low
                             (mem-aref pointer :uint64 1) high)
       #+big-endian (setf (mem-aref pointer :uint64 0) high
                          (mem-aref pointer :uint64 1) low)
       value))
    (8 (setf (mem-ref pointer :uint64) value))
    (4 (setf (mem-ref pointer :uint32) value))
    (2 (setf (mem-ref pointer :uint16) value))
    (1 (setf (mem-ref pointer  :uint8) value))))

(defmacro with-uint-pointer ((pointer bytes &optional value)
                             &body body)
  (once-only (value)
    `(with-foreign-pointer (,pointer ,bytes)
       ,(if value `(setf (mem-ref-uint ,pointer ,bytes) ,value))
       ,@body)))

(defmacro with-uint-pointers ((pointers bytes &optional values)
                             &body body)
  (unless (listp bytes)
    (setf bytes (make-list (length pointers) :initial-element bytes)))
  (if pointers
      `(with-uint-pointer (,(first pointers) ,(first bytes) ,@(ensure-list (car values)))
         (with-uint-pointers (,(rest pointers) ,(rest bytes) ,(cdr values))
           ,@body))
      `(progn ,@body)))

(macrolet ((def (format size)
             (let* ((decimal-size (symbolicate 'decimal- (princ-to-string size)))
                    (format-to-number (symbolicate format '-to-number))
                    (%size-to-number  (symbolicate '% decimal-size '-to-number))
                    (format-from-number (symbolicate 'number-to- format))
                    (%size-from-number  (symbolicate '% decimal-size '-from-number))
                    (number-format (symbolicate 'decnumber- format)))
               `(progn
                  (declaim
                   (ftype (function (foreign-pointer foreign-pointer)
                                    foreign-pointer)
                          ,format-to-number)
                   (ftype (function (foreign-pointer foreign-pointer)
                                    foreign-pointer)
                          ,format-from-number)
                   (inline ,format-to-number ,format-from-number))
                  (defun ,format-to-number (,format context)
                    (declare (inline ,%size-to-number)
                             (ignore context))
                    (let ((number (foreign-alloc ',number-format)))
                      (,%size-to-number ,format number)))
                  (defun ,format-from-number (number context)
                    (declare (inline ,%size-from-number))
                    (let ((,format (foreign-alloc ',format)))
                      (,%size-from-number ,format number context)))))))
  (declare (optimize #+sbcl sb-ext:inhibit-warnings))
  (def single 32)
  (def double 64)
  (def quad  128))

(macrolet ((def (function type)
               (let ((string-size 
                      (case type
                        (number
                         `(+ 5 +decnumber-exponent-digits+
                             (foreign-slot-value number 'decnumber 'digits)))
                        (t (symbolicate '+ type '-string-size+))))
                     (%function (symbolicate '% function)))
                 `(progn
                    (declaim (ftype (function (foreign-pointer) string) ,function))
                    (defun ,function (,type)
                      (declare (inline ,%function))
                      (with-foreign-pointer-as-string (string-pointer ,string-size)
                        (,%function ,type string-pointer)))))))
  
  (def number-to-string     number)
  (def number-to-eng-string number)
  (def single-to-string     single)
  (def single-to-eng-string single)
  (def double-to-string     double)
  (def double-to-eng-string double)
  (def quad-to-string       quad)
  (def quad-to-eng-string   quad))

(macrolet ((def (from to)
               (let ((%function-to-wider (symbolicate '% from '-to-wider))
                     (%function-from-wider (symbolicate '% from '-from-wider)))
                 `(progn
                    (defun ,(symbolicate from '-to- to) (,from context)
                      (let ((,to (foreign-alloc ',to)))
                        (declare (inline ,%function-to-wider)
                                 (ignore context))
                        (,%function-to-wider ,from ,to)))
                    (defun ,(symbolicate to '-to- from) (,to context)
                      (let ((,from (foreign-alloc ',from)))
                        (declare (inline ,%function-from-wider))
                        (,%function-from-wider ,from ,to context)))))))
  (def single double)
  (def double quad))

(defun single-to-quad (single context)
  (declare (inline %single-to-wider %double-to-wider)
           (ignore context))
  (let ((quad (foreign-alloc 'quad)))
    (with-uint-pointer (double 8)
      (%single-to-wider single double)
      (%double-to-wider double quad))))

(defun quad-to-single (quad context)
  (declare (inline %single-from-wider %double-from-wider))
  (let ((single (foreign-alloc 'single)))
    (with-uint-pointer (double 8)
      (%double-from-wider double quad context)
      (%single-from-wider single double context))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun alloc-form (type digits)
    (case type
      (number `(cffi-sys:%foreign-alloc
                (+ (foreign-type-size 'decnumber)
                   (* (1- (ceiling ,digits +decdpun+))
                      +decnumber-unit-size+))))
      (t `(foreign-alloc ',type)))))

(macrolet
    ((def (function (type vars &rest args) &body body)
         `(defun ,function (,@vars ,@args)
            (let ((,type ,(alloc-form type
                                      '(foreign-slot-value context 'context 'digits))))
              ,@body)))
     (def* (inner-type outer-type suffix vars args)
       (let ((function (symbolicate outer-type '- suffix))
             (%function (symbolicate '% inner-type '- suffix))
             (inner-to-outer (symbolicate inner-type '-to- outer-type))
             (outer-to-inner (symbolicate outer-type '-to- inner-type))
             (outer-size (symbolicate '+ outer-type '-pmax+))
             (vars* (mapcar (compose #'gensym #'string) vars)))
         `(defun ,function (,@vars ,@args)
            (declare (inline ,%function))
            (let ((,inner-type ,(alloc-form inner-type outer-size))
                  ,@(mapcar #'list vars*
                            (mapcar (lambda (var)
                                      `(,outer-to-inner ,var context))
                                    vars)))
              (unwind-protect
                   (,inner-to-outer
                    (,%function ,inner-type ,@vars* ,@args)
                    context)
                (foreign-free ,inner-type)
                ,@(mapcar (curry #'list 'foreign-free) vars*))))))
     (defs (types suffixes n-args args)
       (let ((vars (loop for i below n-args
                      collect (symbolicate 'x (princ-to-string i)))))
         `(progn
            ,@(loop for type in types
                 nconcing
                   (loop for suffix in suffixes
                      for function = (symbolicate type '- suffix)
                      for %function = (symbolicate '% function)
                      collect
                        `(def ,function (,type ,vars ,@args)
                           (declare (inline ,%function))
                           (,%function ,type ,@vars ,@args))))
            ,@(unless (member 'single types)
                      (loop for suffix in suffixes
                         collect
                           `(def* double single ,suffix ,vars ,args))))))
     (defs* (suffixes n-args args)
       (let ((vars (loop for i below n-args
                      collect (symbolicate 'x (princ-to-string i)))))
         `(progn
            ,@(loop for suffix in suffixes
                 for function = (symbolicate 'number '- suffix)
                 for %function = (symbolicate '% 'number '- suffix)
                 collect
                   `(def ,function (number ,vars ,@args)
                      (declare (inline ,%function))
                      (,%function number ,@vars ,@args)))
            ,@(loop for type in '(single double quad)
                 nconcing
                   (loop for suffix in suffixes
                      collect
                        `(def* number ,type ,suffix ,vars ,args)))))))

  ;; Convertion from string
  (defs (number single double quad) (from-string) 0 (string context))

  ;; One argument operations
  (defs (number double quad) (abs invert logb minus next-minus next-plus plus
                                  reduce to-integral-exact copy-abs copy-negate
                                  copy-sign)
    1 (context))

  (defs* (log10 ln exp square-root) 1 (context))

  ;; Two arguments operations
  (defs (number double quad) (add and divide divide-integer max max-mag multiply
                                  next-toward or quantize remainder remainder-near
                                  rotate scaleb shift subtract xor compare
                                  compare-signal compare-total compare-total-mag)
    2 (context))

  (defs* (power) 2 (context))

  ;; Three argument operation
  (defs (number double quad) (fma)
    3 (context))

  ;; One argument plus rounding enum
  (defs (number double quad) (to-integral-value)
    1 (context rounding)))

(declaim (ftype (function (foreign-pointer (unsigned-byte 32)
                                           foreign-pointer foreign-pointer)
                          foreign-pointer)
                %number-to-packed %number-from-packed)
         (inline %number-to-packed %number-from-packed))

(locally 
  (declare (optimize #+sbcl sb-ext:inhibit-warnings))
  
  (defun %number-to-packed (bcd length exponent number)
   (declare (inline %packed-from-number))
   (%packed-from-number bcd length exponent number))

  (defun %number-from-packed (bcd length scale number)
    (declare (inline %packed-to-number))
    (%packed-to-number bcd length scale number)))

(macrolet ((def (type suffix new-suffix args &key scale-p)
               (let ((function (symbolicate type '- new-suffix))
                     (%function (symbolicate '% type '- suffix))
                     (array-length
                      (case type
                        (number '(foreign-slot-value number 'decnumber 'digits))
                        (t (symbolicate '+ type '-pmax+)))))
                 (if (eq new-suffix 'to-packed)
                     (setf array-length `(ceiling ,array-length 2)))
                 `(progn
                    (declaim (inline ,function))
                    (defun ,function (,type)
                      (let* ((length ,array-length)
                             (array (make-shareable-byte-vector length)))
                        (declare (inline ,%function))
                        (with-pointer-to-vector-data (bcd array)
                          ,(if (member 'exponent args)
                               `(with-foreign-object (exponent :int32)
                                  (let ((signed-p (,%function ,@args)))
                                    (,%function ,@args)
                                    (values array ,(if scale-p
                                                       `(- (mem-ref exponent :int32))
                                                       `(mem-ref exponent :int32))
                                            signed-p)))
                               `(progn
                                  (,%function ,@args)
                                  (values array
                                          (foreign-slot-value
                                           number 'decnumber 'exponent)
                                          (number-is-signed number)))))))))))
  (def number get-bcd to-bcd (number bcd))
  (def single to-bcd  to-bcd (single exponent bcd))
  (def double to-bcd  to-bcd (double exponent bcd))
  (def quad   to-bcd  to-bcd (quad   exponent bcd))

  (def number to-packed to-packed (bcd length exponent number) :scale-p t)
  (def single to-packed to-packed (single exponent bcd))
  (def double to-packed to-packed (double exponent bcd))
  (def quad   to-packed to-packed (quad   exponent bcd)))

(macrolet ((def (type suffix new-suffix args &key scale-p)
               (let ((function (symbolicate type '- new-suffix))
                     (%function (symbolicate '% type '- suffix))
                     (new-args (remove 'length
                                       (substitute 'exponent 'ptr-exp
                                                   (substitute 'array 'bcd args)))))
                 (unless (member 'exponent new-args)
                   (nconcf new-args '(exponent)))
                 `(progn
                    (declaim (inline ,function))
                    (defun ,function (array exponent
                                      ,@(if (eq 'from-bcd new-suffix)
                                            `(signed-p)))
                      (declare (inline ,%function))
                      (with-pointer-to-vector-data (bcd array)
                        (let* ,(if (eq 'number type)
                                   `((length (array-dimension array 0))
                                     (,type ,(alloc-form type
                                                         (if (eq new-suffix 'from-packed)
                                                             '(* length 2)
                                                             'length))))
                                   `((,type ,(alloc-form type 0))))
                          ,(if (member 'ptr-exp args)
                               `(with-foreign-object (ptr-exp :int32)
                                  (setf (mem-ref ptr-exp :int32)
                                        ,(if scale-p '(- exponent) 'exponent))
                                  ,(if (eq 'from-bcd new-suffix)
                                       `(let ((sign (if signed-p +decfloat-neg+ 0)))
                                          (,%function ,@args sign))
                                       `(,%function ,@args)))
                               `(progn
                                  (setf (foreign-slot-value number 'decnumber 'exponent)
                                        exponent)
                                  (prog1
                                      (,%function ,@args)
                                    (if signed-p
                                        (setf (foreign-slot-value number 'decnumber 'bits)
                                              +flag-neg+))))))))))))
  (declare (optimize #+sbcl sb-ext:inhibit-warnings))
  
  (def number set-bcd  from-bcd (number bcd length))
  (def single from-bcd from-bcd (single ptr-exp bcd))
  (def double from-bcd from-bcd (double ptr-exp bcd))
  (def quad   from-bcd from-bcd (quad   ptr-exp bcd))

  (def number from-packed from-packed (bcd length ptr-exp number) :scale-p t)
  (def single from-packed from-packed (single ptr-exp bcd))
  (def double from-packed from-packed (double ptr-exp bcd))
  (def quad   from-packed from-packed (quad   ptr-exp bcd)))
