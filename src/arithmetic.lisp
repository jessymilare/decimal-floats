;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defmacro defop (name (&rest vars) (&optional (condition (gensym "CONDITION"))
                                              &rest condition-case)
                 &body body)
  (multiple-value-bind (body declarations documentation)
      (parse-body body :documentation t)
    (setf body (cons 'progn body))
    (loop for (varspec . tail) on (nreverse (subseq vars 0 (position '&others vars)))
       for var = (car varspec)
       and keys = (cdr varspec) do
         (with-gensyms (result)
           (unless (getf keys :qnan)
             (setf (getf keys :qnan) var))
           (setf body
                 `(with-inf-nan-handler
                      (,var :nan (let ((,result ,var))
                                   ;; Spec says to copy information
                                   ;; from the first sNaN, if any, or
                                   ;; of the first qNaN.
                                   (if (or (df-infinity-p ,var)
                                           ,@(mapcar (lambda (varspec)
                                                       (let ((var (car varspec)))
                                                         `(and (df-not-a-number-p ,var)
                                                               (df-infinity-p ,var)
                                                               (progn (setf ,result ,var)
                                                                      t))))
                                                     tail))
                                       (decimal-error-cond
                                           ((cond
                                              ((df-slots ,result)
                                               (let ((,result (copy-decimal ,result)))
                                                 (setf (df-infinity-p ,result) nil)
                                                 ,result))
                                              ((df-negative-p ,result) +-qnan+)
                                              (t ++qnan+)))
                                         (decimal-invalid-operation))
                                       (call-next-handler)))
                            ,@keys)
                    ,body))))
    (deletef vars '&others)
    `(defun ,name ,(mapcar #'ensure-car vars)
       ,@(ensure-list documentation)
       ,@declarations
       (with-operation (,name ,condition ,@(mapcar #'ensure-car vars))
           (,@condition-case)
         ,body))))

(defop logb-int ((x :infinity ++infinity+))
    ()
  (if (df-zero-p x)
      (decimal-error-cond (+-infinity+)
        decimal-division-by-zero)
      (df-logb x)))

(defun logb (x)
  (declare (inline integer-to-decimal))
  (integer-to-decimal (logb-int x) :round-p t))

(defun %scaleb (x scale)
  (if (decimal-float-p scale)
      (setf scale (decimal-to-integer scale)))
  (unless (<= (abs scale) (* 2 (+ +maximum-exponent+ *precision*)))
    (decimal-error-cond (++qnan+)
      decimal-invalid-operation))
  (let* ((slots (df-slots x))
         (length (length slots))
         (iexponent (df-iexponent x))
         (extra (df-extra x))
         (fsld (%df-first-slot-last-digit extra))
         (lsfd (%df-last-slot-first-digit extra)))
    (multiple-value-bind (iexp-offset digits-offset)
        (floor scale +decimal-slot-digits+)
      (let* ((iexponent (+ iexponent iexp-offset))
             (new-fsld (+ fsld digits-offset))
             (new-lsfd (+ lsfd digits-offset))
             (new-length length)
             (start 0)
             (prev-high 0)
             (pow-10  (aref +expt-10+ (- +decimal-slot-digits+ digits-offset)))
             (pow-10* (aref +expt-10+ digits-offset)))
        (when (>= new-lsfd +decimal-slot-digits+)
          (decf new-lsfd +decimal-slot-digits+)
          (incf new-length)
          (incf iexponent))
        (when (>= new-fsld +decimal-slot-digits+)
          (decf new-fsld +decimal-slot-digits+)
          (decf new-length)
          (setf start 1
                prev-high (truncate (aref slots 0) pow-10)))
        (let ((new-slots (make-digits-array new-length))
              (*precision* (%df-count-digits length fsld lsfd)))
          (loop with high = 0 and low = 0
             for i from start below length
             for j from 0
             do
               (setf (values high low) (truncate (aref slots i) pow-10)
                     (aref new-slots j) (+ prev-high (* low pow-10*))
                     prev-high high)
             finally (unless (zerop prev-high)
                       (setf (aref new-slots (1- new-length)) prev-high)))
          (normalize-number iexponent new-slots new-fsld (%df-negative-p extra)))))))

(defop scaleb ((x :infinity x) &others scale)
    ()
  (%scaleb x scale))
