;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(define-constant ++infinity+ (let ((x (%make-df nil 0)))
                               (setf (df-infinity-p x) t)
                               x)
  :test 'equalp)

(define-constant +-infinity+ (let ((x (%make-df nil 0)))
                               (setf (df-infinity-p x) t
                                     (df-negative-p x) t)
                               x)
  :test 'equalp)

(define-constant ++qnan+ (make-nan nil nil)
  :test 'equalp)

(define-constant +-qnan+ (make-nan t nil)
  :test 'equalp)

(define-constant ++snan+ (make-nan nil t)
  :test 'equalp)

(define-constant +-snan+ (make-nan t t)
  :test 'equalp)

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
         ,@(ensure-list documentation)
         ,@declarations
         (case ,key
           ,@defaults
           (t (funcall ,key ,@lambda-list)))))))

(defun prompt (stream control-string &rest format-arguments)
  (apply #'format stream control-string format-arguments)
  (read))

(defun map-digits-array (function slots fsld lsfd update-p)
  ;; Maps the digits, from most significant one to least significant one
  ;; and changes them in-place unless update-p is nil.
  (let ((last-slot (1- (length slots))))
    (loop
       for nslot from last-slot downto 0
       for slot = (aref slots nslot)
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
           (setf (aref slots nslot) (if (zerop nslot)
                                        (* acc (aref +expt-10+ end))
                                        acc)))))
  slots)

(defmacro %with-inf-nan-handler ((extra &key
                                        (any '(call-next-handler))
                                        (infinity '(call-next-handler))
                                        (+infinity nil)
                                        (-infinity nil)
                                        (nan '(call-next-handler))
                                        (qnan '(call-next-handler))
                                        (+qnan nil)
                                        (-qnan nil)
                                        (snan '(call-next-handler))
                                        (+snan nil)
                                        (-snan nil))
                                 &body body)
  (once-only (extra)
    `(if (%df-finite-p ,extra)
         (progn ,@body)
         (flet ((call-next-handler ()
                  (cond
                    ((%df-not-a-number-p ,extra)
                     (flet ((call-next-handler ()
                              (cond
                                ;; in a NaN, this flag differs
                                ;; signaling NaN from quiet NaN
                                ((%df-infinity-p ,extra)
                                 (flet ((call-next-handler ()
                                          (cond
                                            ((%df-negative-p ,extra)
                                             ,-snan)
                                            (t
                                             ,+snan))))
                                   (declare (inline call-next-handler)
                                            (ignorable #'call-next-handler))
                                   ,snan))
                                (t
                                 (flet ((call-next-handler ()
                                          (cond
                                            ((%df-negative-p ,extra)
                                             ,-qnan)
                                            (t
                                             ,+qnan))))
                                   (declare (inline call-next-handler)
                                            (ignorable #'call-next-handler))
                                   ,qnan)))))
                       (declare (inline call-next-handler)
                                (ignorable #'call-next-handler))
                       ,nan))
                    (t
                     (flet ((call-next-handler ()
                              (cond
                                ((%df-negative-p ,extra)
                                 ,-infinity)
                                (t
                                 ,+infinity))))
                       (declare (inline call-next-handler)
                                (ignorable #'call-next-handler))
                       ,infinity)))))
           (declare (inline call-next-handler)
                    (ignorable #'call-next-handler))
           ,any))))

(defmacro with-inf-nan-handler ((x &rest all-keys
                                   &key
                                   (any '(call-next-handler))
                                   (infinity '(call-next-handler))
                                   (+infinity nil)
                                   (-infinity nil)
                                   (nan '(call-next-handler))
                                   (qnan '(call-next-handler))
                                   (+qnan nil)
                                   (-qnan nil)
                                   (snan '(call-next-handler))
                                   (+snan nil)
                                   (-snan nil))
                                &body body)
  (declare (ignore any infinity +infinity -infinity nan
                   qnan +qnan -qnan snan +snan -snan))
  `(%with-inf-nan-handler ((df-extra ,x) ,@all-keys) ,@body))

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
