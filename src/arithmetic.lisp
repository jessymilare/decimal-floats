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

(defop logb ((x :infinity ++infinity+))
    ()
  (if (df-zero-p x)
      (decimal-error-cond (+-infinity+)
        decimal-division-by-zero)
      (df-logb x)))
