;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

;;; Inpired by SBCL's sb-bignum source code.

(defconstant +binary-carry-value+ (rem (expt 2 +decimal-slot-bits+)
                                       (expt 10 +decimal-slot-digits+)))


(declaim (inline %addc %subc)
         (ftype (function (decimal-slot decimal-slot bit)
                          (values bit decimal-slot))
                %addc %subc))

(defun %addc (x y carry)
  (declare (optimize speed))
  (incf x carry)
  (let ((cy (- (expt 10 +decimal-slot-digits+) y)))
    (declare (type decimal-slot cy))
    (if (< x cy)
        (values 0 (+ x y))
        (values 1 (- x cy)))))

(defmacro %addcf (x y carry)            ; (carry,x):=x+y+carry
  `(setf (values ,carry ,x) (%addc ,x ,y ,carry)))

(defun %subc (x y carry)
  (declare (optimize speed))
  (incf y carry)
  (if (<= y x)
      (values 0 (- x y))
      (values 1 (- (expt 10 +decimal-slot-digits+)
                   (the decimal-slot
                     (- y x))))))

(defmacro %subcf (x y carry)
  `(setf (values ,carry ,x) (%subc ,x ,y ,carry)))

#+(or cmucl sbcl)
(defconstant +R+ (rem (expt 2 (+ +decimal-slot-bits+ +decimal-slot-digits+))
                      (expt 10 +decimal-slot-digits+)))

#+(or cmucl sbcl)
(defconstant +S+ (truncate (expt 2 (+ +decimal-slot-bits+ +decimal-slot-digits+))
                           (expt 10 +decimal-slot-digits+)))

(defmacro with-%mul-addc (&body body)
  `(flet
       (;; Default implementation.
        #-(or cmucl sbcl)
        (%mul-addc (x y d carry) ; d+x*y+carry -> (values new-carry result)
          (truncate (+ (* x y) d carry) (expt 10 +decimal-slot-digits+)))

        ;; Optimization using internal bignum arithmetic. About 5 times faster.
        #+(or cmucl sbcl)
        (%mul-addc (x y d carry)
          (declare (optimize speed space))
          (multiple-value-bind (aux1 digit0)
              (#+sbcl sb-bignum:%multiply-and-add
               #+cmucl bignum:%multiply-and-add
                 x y d carry)
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
                   (if (>= digit0 (expt 10 +decimal-slot-digits+))
                       (values (1+ digit1) (- digit0 (expt 10 +decimal-slot-digits+)))
                       (values digit1 digit0)))
                (setf (values high low) (truncate (the decimal-slot aux1)
                                                  (expt 2 +decimal-slot-digits+))
                      (values aux1 aux0) (truncate (the unsigned-word
                                                     (ash low (- +decimal-slot-bits+
                                                                 +decimal-slot-digits+)))
                                                   (expt 5 +decimal-slot-digits+))
                      digit1 (+ digit1 aux1)
                      digit1 (+ digit1 (the decimal-slot (* high +S+)))
                      (values aux1 digit0)
                      (#+sbcl sb-bignum:%multiply-and-add
                       #+cmucl bignum:%multiply-and-add
                       high +R+ digit0 (ash aux0 +decimal-slot-digits+))))))))
     (declare (inline %mul-addc)
              (ftype (function (decimal-slot decimal-slot decimal-slot decimal-slot)
                               (values decimal-slot decimal-slot))
                     %mul-addc))
     ,@body))

(defmacro %mul-addcf (x y d carry)       ; (carry,d):=d+x*y+carry)
  `(setf (values ,carry ,d) (mul-addc ,d ,x ,y ,carry)))

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
          ,(format nil "Fetches the value of ~:@(~A~) and parses it to a better format.
 See documentation for ~:@(~A~) and ~:@(~A~)." var var find-)
          ,@body)
        (defsetf ,get- (&optional (,name ',var)) (,value)
          (once-only (,value)
            `(progn
               (setf ,,name (,',find- ,,value))
               ,,value)))
        (defmacro ,with- ((,value) &body ,body-var)
          ,(format nil "Transforms the ~:@(~A~) given into an internal format (using the function ~:@(~A~))
 and locally binds the returned value to ~:@(~A~).
 See documentation for ~:@(~A~) and ~:@(~A~)." value find- var var find-)
          `(let ((,',var (,',find- ,,value)))
             ,@,body-var))))))

(defmacro def-customize-function ((name default-value) lambda-list variable-doc &rest defaults)
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
        new-slots))
  #+nil
  (cond
    ((zerop delta)
     (if (plusp start)
         (subseq slots start)
         slots))
    ((plusp delta)
     (adjust-array (if (plusp start)
                       (subseq slots start)
                       slots)
                   new-length :initial-element 0))
    (t
     (subseq slots start new-length))))

(declaim (inline whitespace-p))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline #\Return #\Tab)))
