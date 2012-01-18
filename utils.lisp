;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

;;; Inpired by SBCL's sb-bignum source code.

;;; It's ugly defining these as macros, but it increases performance a lot

(defmacro %addc (x y carry)
  (once-only (x y carry)
    (with-gensyms (cy)
      ;; Avoid overflows of fixnums and is as fast as
      ;; (truncate (+ x y carry) +maximum-decimal-slot+)
      ;; on SBCL
      `(let ((,x (+ ,x ,carry))
             (,cy (- +maximum-decimal-slot+ ,y)))
         (declare (type fixnum ,x ,cy)
                  (type decimal-slot ,y)
                  (type bit ,carry)
                  (optimize speed))
         (if (< ,x ,cy)
             (values 0 (the decimal-slot (+ ,x ,y)))
             (values 1 (the decimal-slot (- ,x ,cy))))))))

(defmacro %addcf (x y carry)            ; (carry,x):=x+y+carry
  `(setf (values ,carry ,x) (%addc ,x ,y ,carry)))

(defun %subc (x y carry)
  (once-only (x y carry)
    ;; Avoids overflow of fixnums and is as fast as
    ;; (truncate (- x y carry) +maximum-decimal-slot+)
    ;; on SBCL
    ;; The carry has negative sign
    `(let ((,y (+ ,y ,carry)))
       (declare (type fixnum ,y)
                (type decimal-slot ,x)
                (type bit ,carry)
                (optimize speed))
       (if (<= ,y ,x)
           (values 0 (- ,x ,y))
           (values 1 (- +maximum-decimal-slot+
                        (the decimal-slot
                          (- ,y ,x))))))))

(defmacro %subcf (x y carry)
  `(setf (values ,carry ,x) (%subc ,x ,y ,carry)))

(defconstant +half-decimal-digits+ (/ +decimal-slot-digits+ 2))

(check-type +half-decimal-digits+ 'fixnum)

(defconstant +half-maximum-slot+ (expt 10 +half-decimal-digits+))

(defmacro %mul-addc (x y d c)
  (once-only (x y d c)
    (with-gensyms (x0 x1 y0 y1 low high carry a0 a1)
      ;; Avoids overflow of fixnums, equivalent to
      ;; (truncate (+ (* x y) d c) +maximum-decimal-slot+)
      (declare (ignorable carry))
      `(locally (declare (type decimal-slot ,x ,y ,d ,c)
                         (optimize speed))
         (multiple-value-bind (,x1 ,x0) (truncate ,x +half-maximum-slot+)  
           (multiple-value-bind (,y1 ,y0) (truncate ,y +half-maximum-slot+)
             (let ((,a0 0) (,a1 0)
                   (,high (* ,x1 ,y1)) (,low 0))
               (declare (type (mod ,+half-maximum-slot+) ,a0 ,a1)
                        (type decimal-slot ,high))
               #-(or sbcl cmucl)
               (let ((,carry 0))
                 (declare (type bit ,carry)
                          (type decimal-slot ,low))
                 (setf (values ,carry ,low) (%addc ,d ,c 0)
                       ,high (+ ,high ,carry)
                       (values ,carry ,low) (%addc ,low (* ,x0 ,y0) 0)
                       ,high (+ ,high ,carry)
                       (values ,a1 ,a0)  (truncate (* ,x0 ,y1) +half-maximum-slot+)
                       (values ,carry ,low) (%addc ,low (* ,a0 +half-maximum-slot+) 0)
                       ,high (+ ,high ,carry ,a1)
                       (values ,a1 ,a0)  (truncate (* ,x1 ,y0) +half-maximum-slot+)
                       (values ,carry ,low) (%addc ,low (* ,a0 +half-maximum-slot+) 0))
                 (values (+ ,high ,carry ,a1)
                         ,low))
               ;; This version assumes the compiler knows how to sum fixnums
               ;; without using bignums; about 10% faster then above
               #+(or sbcl cmucl)
               (progn
                 (setf ,low  (+ ,d ,c (* ,x0 ,y0))
                       (values ,a1 ,a0)  (truncate (* ,x0 ,y1) +half-maximum-slot+)
                       ,low (+ ,low (* ,a0 +half-maximum-slot+))
                       ,high (+ ,high ,a1)
                       (values ,a1 ,a0)  (truncate (* ,x1 ,y0) +half-maximum-slot+)
                       ,low (+ ,low (* ,a0 +half-maximum-slot+))
                       ,high (+ ,high ,a1)
                       (values ,a1 ,low) (truncate ,low +maximum-decimal-slot+))
                 (values (+ ,high ,a1)
                         ,low)))))))))

(defmacro %mul-addcf (x y d carry)       ; (carry,d):=d+x*y+carry)
  `(setf (values ,carry ,d) (%mul-addc ,d ,x ,y ,carry)))

(macrolet ((def (name (var signed-p &rest args) &body body)
	     `(defun ,(symbolicate "MAKE-" name) (,signed-p ,@args)
                (let ((,var (%make-df nil 0)))
                  ,@body
                  (setf (df-negative-p ,var) ,signed-p)
                  ,var))))
  (def infinity (x signed-p)
    (setf (df-infinity-p x) t))
  (def qnan (x signed-p condition)
    (setf (df-not-a-number-p x) t
          (df-iexponent x) condition))
  (def snan (x signed-p condition)
    (setf (df-not-a-number-p x) t
          (df-infinity-p x) t
          (df-iexponent x) condition)))

(defmacro def-var-get-and-with ((name default-value) &body body)
  (let ((find- (symbolicate 'find- name))
        (get- (symbolicate 'get- name))
        (with- (symbolicate 'with- name))
        (var (symbolicate "*" name "*"))
        (doc (if (stringp (first body))
                 (pop body))))
    (with-gensyms (value body-var)
     `(progn
        (defvar ,var (,find- ,default-value)
          ,@(if doc (list doc)))
        (defun ,get- (&optional (,name ,var))
          ,(format nil "Fetches the value of ~:@(~A~) and parses it into a better format.
 See documentation for ~:@(~A~) and ~:@(~A~)." var var find-)
          ,@body)
        (defsetf ,get- (&optional (,name ',var)) (,value)
          (once-only (,value)
            `(progn
               (setf ,,name (,',find- ,,value))
               ,,value)))
        (defmacro ,with- ((,value) &body ,body-var)
          ,(format nil "Transforms the ~:@(~A~) given into an internal format~
 (using the function ~:@(~A~))
 and locally binds the returned value to ~:@(~A~).
 See documentation for ~:@(~A~) and ~:@(~A~)." value find- var var find-)
          `(let ((,',var (,',find- ,,value)))
             ,@,body-var))))))

(defmacro def-customize-function ((name default-value) lambda-list variable-doc
                                  &rest defaults)
  (with-gensyms (key)
    (let (declaration)
      `(progn
         (defun ,(symbolicate 'find- name) (,key)
           ,@(if (stringp (first defaults)) ; documentation
                 (list (pop defaults)))
           ,@(when (eq 'declare (caar defaults))
               (setf declaration (pop defaults))
               nil)
           (if (functionp ,key)
               ,key
               (ecase ,key
                 ,@(loop for (key . body) in defaults
                      collect `(,key (named-lambda ,key ,lambda-list
                                       (declare (ignorable ,@lambda-list))
                                       ,declaration
                                       ,@body))))))
         (def-var-get-and-with (,name ,default-value) ,variable-doc ,name)))))

(defun prompt (stream control-string &rest format-arguments)
  (apply #'format stream control-string format-arguments)
  (read))

(defmacro with-inf-nan-handler ((var &key
				     after before
                                     (around '(call-next-handler))
				     inf-before inf-after
				     (inf-around '(call-next-handler))
				     (+infinity nil +infinity-p)
				     (-infinity nil -infinity-p)
				     nan-before nan-after
				     (nan-around '(call-next-handler))
				     (qnan nil qnan-p)
				     (snan nil snan-p))
				&body body)
  (check-type var symbol)
  `(if (df-finite-p ,var)
       (progn ,@body)
       (flet ((call-next-handler ()
                (prog2
                    ,before
                    (cond
                      ((df-not-a-number-p ,var)
                       (flet ((call-next-handler ()
                                (prog2
                                    ,nan-before
                                    ,(if (or snan-p qnan-p)
                                         `(cond
                                            ;; in a NaN, this flag differs
                                            ;; signaling NaN from quiet NaN
                                            ((df-infinity-p ,var)
                                             ,snan)
                                            (t
                                             ,qnan)))
                                  ,nan-after)))
                         (declare (inline call-next-handler))
                         ,nan-around))
                      ((df-infinity-p ,var)
                       (flet ((call-next-handler ()
                                (prog2
                                    ,inf-before
                                    ,(if (or +infinity-p -infinity-p)
                                         `(cond
                                            ((df-negative-p ,var)
                                             ,-infinity)
                                            (t
                                             ,+infinity)))
                                  ,inf-after)))
                         (declare (inline call-next-handler))
                         ,inf-around)))
                  ,after)))
         (declare (inline call-next-handler))
         ,around)))

(defun resize-slots (slots start end)
  (declare (type slot-array slots)
           (fixnum start end))
  (if (zerop start)
      #+sbcl ;; sbcl does not allocate a new array if it already has the specified length
      (adjust-array slots end :initial-element 0)
      #-sbcl
      (if (= end (length slots))
          slots
          (adjust-array slots end :initial-element 0))
      (let ((new-slots (make-array (- end start) :element-type 'decimal-slot)))
        (loop for i from (max start 0) below (min (length slots) end)
           do
             (setf (aref new-slots (- i start)) (aref slots i)))
        new-slots)))

(declaim (inline whitespace-p))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Return #\Tab)))
