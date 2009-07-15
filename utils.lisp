;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :decimal-floats)

;;; Inpired by SBCL's sb-bignum source code.

(declaim (ftype (function (decimal-slot decimal-slot bit)
                          (values bit decimal-slot))
                %addc))

#+sbcl
(defconstant +R+ (rem (expt 2 (+ +decimal-slot-bits+ +decimal-slot-digits+))
                      (expt 10 +decimal-slot-digits+)))

#+sbcl
(defconstant +S+ (truncate (expt 2 (+ +decimal-slot-bits+ +decimal-slot-digits+))
                           (expt 10 +decimal-slot-digits+)))

#+(and sbcl x86-64)
(defconstant +binary-carry-value+ (rem (expt 2 64) (expt 10 +decimal-slot-digits+)))

#-(and sbcl x86-64)
;;; Default implementation.
(defun %addc (x y carry) ; x+y+carry -> (values new-carry result)
  (declare (optimize speed (space 0)))
  (truncate (+ x y carry) (expt 10 +decimal-slot-digits+)))

;;; Optimization using internal sb-bignum arithmetic.
;;; It seems to be almost 5 times faster, but occupies 3 times more space,
;;; even with the space declaration.
#+(and sbcl x86-64)
(defun %addc (x y carry)
  ;; No sensible performance penalty for space declaration.
  (declare (optimize speed space))
  (multiple-value-bind (value binary-carry)
      (sb-bignum:%add-with-carry x y carry)
    (cond
      ((>= value (expt 10 +decimal-slot-digits+))
       (values 1 (- value (expt 10 +decimal-slot-digits+))))
      ((= 1 binary-carry)
       (values 1 (+ +binary-carry-value+ value)))
      (t (values 0 value)))))

(defmacro %addcf (x y carry)            ; (carry,x):=x+y+carry
  `(setf (values ,carry ,x) (%addc ,x ,y ,carry)))

(declaim (ftype (function (decimal-slot decimal-slot decimal-slot decimal-slot)
                          (values decimal-slot decimal-slot))
                %mul-add))

;;; Default implementation.
#-sbcl
(defun %mul-add (x y d carry) ; d+x*y+carry -> (values new-carry result)
  (truncate (+ (* x y) d carry) (expt 10 +decimal-slot-digits+)))

#+sbcl
;;; Another optimization, also using sbcl's internals
;;; and also about 5 times faster
(defun %mul-add (x y d carry)
  (declare (optimize speed space))
  (multiple-value-bind (aux1 digit0)
      (sb-bignum:%multiply-and-add x y d carry)
    (let ((low 0)
          (high 0)
          (digit1 0)
          (aux0 0))
      (declare (type unsigned-word digit0)
               (type decimal-slot digit1 aux1 aux0)
               (type (unsigned-byte #.(- +decimal-slot-bits+
                                         +decimal-slot-digits+))
                     high))
      (do ()
          ((zerop aux1)
           #+x86-64
           (if (>= digit0 (expt 10 +decimal-slot-digits+))
               (values (1+ digit1) (- digit0 (expt 10 +decimal-slot-digits+)))
               (values digit1 digit0))
           #-x86-64
           (multiple-value-bind (delta digit0)
               (truncate digit0 (expt 10 +decimal-slot-digits+))
             (values (+ delta digit1) digit0)))
        (setf (values high low) (truncate (the decimal-slot aux1)
                                          (expt 2 +decimal-slot-digits+))
              (values aux1 aux0) (truncate (the unsigned-word
                                             (ash low (- +decimal-slot-bits+
                                                         +decimal-slot-digits+)))
                                           (expt 5 +decimal-slot-digits+))
              digit1 (+ digit1 aux1)
              digit1 (+ digit1 (the decimal-slot (* high +S+)))
              (values aux1 digit0)
              (sb-bignum:%multiply-and-add high +R+
                                           digit0 (ash aux0 +decimal-slot-digits+)))))))

(defmacro %mul-addf (x y d carry)       ; (carry,d):=d+x*y+carry)
  `(setf (values ,carry ,d) (mul-add ,d ,x ,y ,carry)))

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
          (df-slots x) condition))
  (def snan (x signed-p condition)
    (setf (df-not-a-number-p x) t
          (df-infinity-p x) t
          (df-slots x) condition)))

(declaim (inline call-customize-function))

(defun call-customize-function (table-function fn-or-key &rest args)
  (apply (if (functionp fn-or-key)
             fn-or-key
             (funcall table-function fn-or-key))
         args))

(defmacro def-customize-function (table-function lambda-list &rest defaults)
  (with-gensyms (key)
    `(defun ,table-function (,key)
       ,@(if (stringp (first defaults))  ; documentation
             (list (pop defaults)))
       (ecase ,key
         ,@(loop for (key . body) in defaults
              collect `(,key (named-lambda ,key ,lambda-list
                               (declare (ignorable ,@lambda-list))
                              ,@body)))))))

(defmacro with-inf-nan-handler ((var &key
				     after
				     before
                                     (around '(call-next-handler))
				     inf-before
				     inf-after
				     (inf-around '(call-next-handler))
				     (+infinity nil +infinity-p)
				     (-infinity nil -infinity-p)
				     nan-before
				     nan-after
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
                                            ;; in a NaN, this flag differs signaling NaN from quiet NaN
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
