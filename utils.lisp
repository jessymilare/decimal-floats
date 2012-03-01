;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

;;; Inpired by SBCL's sb-bignum source code.

(declaim (inline %addc %subc %mul-addc))

(defun %addc (x y carry)
  ;; Avoid overflows of fixnums and is as fast as
  ;; (truncate (+ x y carry) +maximum-decimal-slot+)
  ;; on SBCL.
  (let ((x (+ x carry))
        (cy (- +maximum-decimal-slot+ y)))
    (declare (type fixnum x cy)
             (type decimal-slot y)
             (type bit carry)
             (optimize speed))
    (if (< x cy)
        (values 0 (the decimal-slot (+ x y)))
        (values 1 (the decimal-slot (- x cy))))))

(defmacro %addcf (x y carry)            ; (carry,x):=x+y+carry
  `(setf (values ,carry ,x) (%addc ,x ,y ,carry)))

(defun %subc (x y carry)
  ;; Avoids overflow of fixnums and is as fast as
  ;; (truncate (- x y carry) +maximum-decimal-slot+)
  ;; on SBCL.
  ;; The carry has negative sign.
  (let ((y (+ y carry)))
    (declare (type decimal-slot x y)
             (type bit carry)
             (optimize speed))
    (if (<= y x)
        (values 0 (- x y))
        (values 1 (- +maximum-decimal-slot+
                     (the decimal-slot
                       (- y x)))))))

(defmacro %subcf (x y carry)
  `(setf (values ,carry ,x) (%subc ,x ,y ,carry)))

(defconstant +half-decimal-digits+ (/ +decimal-slot-digits+ 2))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (assert (typep +half-decimal-digits+ 'fixnum)))

(defconstant +half-maximum-slot+ (expt +base+ +half-decimal-digits+))

(defun %mul-addc (x y d c)
  ;; Avoids overflow of fixnums, equivalent to
  ;; (truncate (+ (* x y) d c) +maximum-decimal-slot+).
  (declare (type decimal-slot x y d c))
  (multiple-value-bind (x1 x0) (truncate x +half-maximum-slot+)  
    (multiple-value-bind (y1 y0) (truncate y +half-maximum-slot+)
      (let ((a0 0) (a1 0)
            (high (* x1 y1)) (low 0))
        (declare (type (mod #.+half-maximum-slot+) a0 a1)
                 (type decimal-slot high))
        (setf low  (+ d c (* x0 y0))
              (values a1 a0)  (truncate (* x0 y1) +half-maximum-slot+)
              low (+ low (* a0 +half-maximum-slot+))
              high (+ high a1)
              (values a1 a0)  (truncate (* x1 y0) +half-maximum-slot+)
              low (+ low (* a0 +half-maximum-slot+))
              high (+ high a1)
              (values a1 low) (truncate low +maximum-decimal-slot+))
        (values (+ high a1)
                low)))))

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

(defmacro def-customize-function (&whole whole name lambda-list &body body)
  (with-gensyms (key)
    (multiple-value-bind (defaults declarations documentation)
        (parse-body body :documentation t :whole whole)
      `(defun ,name (,key ,@lambda-list)
         ,@(when documentation `(,documentation))
         ,@declarations
         (case ,key
           ,@defaults
           (t (funcall ,key ,@lambda-list)))))))

(defun prompt (stream control-string &rest format-arguments)
  (apply #'format stream control-string format-arguments)
  (read))


(defun map-digits-array (function x-slots fsld lsfd update-p)
  ;; Maps the digits, from most significant one to least significant one
  ;; and changes them in-place unless update-p is nil.
  (let ((last-slot (1- (length x-slots))))
    (loop
       for nslot from last-slot downto 0
       for slot = (aref x-slots nslot)
       for start = lsfd then (1- +decimal-slot-digits+)
       for end   = (if (= nslot 0)
                       fsld
                       0)
       for acc = 0
       do
         (loop
            for i from start downto end
            do
              (multiple-value-bind (next-digit next-slot)
                  (truncate slot (aref +expt-10+ i))
                (declare ((mod #.+base+) next-digit))
                (if update-p
                    (setf acc (+ (* acc +base+)
                                 (the (mod #.+base+) (funcall function next-digit))))
                    (funcall function next-digit))
                (setf slot next-slot)))
         (when update-p
           (setf (aref x-slots nslot) (if (zerop nslot)
                                          (* acc (aref +expt-10+ end))
                                          acc)))))
  x-slots)

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

(declaim (inline resize-slots))
(defun resize-slots (slots start end)
  (declare (type slot-array slots)
           (fixnum start end))
  (if (and (zerop start) (= end (length slots)))
      slots
      (%resize-slots slots start end)))

(defun %resize-slots (slots start end)
  (let ((new-slots (make-array (- end start) :element-type 'decimal-slot
                               :initial-element 0)))
    (loop for i from (max start 0) below (min (length slots) end) do
         (setf (aref new-slots (- i start)) (aref slots i)))
    new-slots))

(declaim (inline whitespace-p))
(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Return #\Tab)))
