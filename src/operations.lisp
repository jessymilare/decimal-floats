;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defmacro defop (name (&rest vars) (&optional (condition (gensym "CONDITION"))
                                              &rest condition-case)
                 &body body)
  (with-gensyms (result get-result)
    (multiple-value-bind (body declarations documentation)
        (parse-body body :documentation t)
      (setf body (cons 'progn body))
      (loop with dec-vars = (mapcar #'ensure-list
                                    (subseq vars 0 (position '&others vars)))
         for n from 1 to (length dec-vars)
         for (varspec . tail) = (last dec-vars n)
         for var = (car varspec)
         and keys = (cdr varspec)
         do
           (unless (getf keys :qnan)
             (setf (getf keys :qnan) var))
           (setf body
                 `(with-inf-nan-handler
                      (,var
                       :nan (let ((,result ,var))
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
                                  (,get-result ,result)
                                  ,(or (getf keys :nan)
                                       '(call-next-handler))))
                       :infinity
                       (let ((,result ,var))
                         (if (or ,@(maplist
                                    (lambda (varspecs)
                                      (let* ((var (caar varspecs)))
                                        `(and (df-not-a-number-p ,var)
                                              (progn
                                                (setf ,result ,var)
                                                ,@(mapcar
                                                   (lambda (varspec)
                                                     (let ((var (car varspec)))
                                                       `(and (df-not-a-number-p ,var)
                                                             (df-infinity-p ,var)
                                                             (progn
                                                               (setf ,result ,var)
                                                               t))))
                                                   (cdr varspecs))
                                                t))))
                                    tail))
                             (if (df-infinity-p ,result)
                                 (,get-result ,result)
                                 ,result)
                             ,(or (getf keys :infinity)
                                  '(call-next-handler))))
                       ,@(progn
                          (remf keys :infinity)
                          (remf keys :nan)
                          keys))
                    ,body)))
      (deletef vars '&others)
      `(defun ,name ,(mapcar #'ensure-car vars)
         ,@(ensure-list documentation)
         ,@declarations
         (with-operation (,name ,condition ,@(mapcar #'ensure-car vars))
             (,@condition-case)
           (flet ((,get-result (,result)
                    (decimal-error-cond
                        ((cond
                           ((df-slots ,result)
                            (let ((,result (copy-decimal ,result)))
                              (setf (df-infinity-p ,result) nil)
                              ,result))
                           ((df-negative-p ,result) +-qnan+)
                           (t ++qnan+)))
                      (decimal-invalid-operation))))
             ,body))))))

(defop logb-int ((x :infinity ++infinity+))
    ()
  (if (df-zero-p x)
      (decimal-error-cond (+-infinity+)
        decimal-division-by-zero)
      (df-logb x)))

(defun logb (x)
  (declare (inline integer-to-decimal))
  (let ((result (logb-int x)))
    (if (integerp result)
        (integer-to-decimal result :round-p t)
        result)))

(defun %scaleb-int (x scale)
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

(defop scaleb-int ((x :infinity x) &others scale)
    ()
  (declare (type integer scale))
  (unless (<= (abs scale) (* 2 (+ +maximum-exponent+ *precision*)))
    (decimal-error-cond (++qnan+ :return-p t)
      decimal-invalid-operation))
  (%scaleb-int x scale))

(defop scaleb ((x :infinity
                  (if (zerop (df-exponent scale))
                      x
                      ;; SCALE is not an integer! Abort.
                      (decimal-error-cond (++qnan+)
                        decimal-invalid-operation)))
               (scale :infinity (decimal-error-cond (++qnan+)
                                  decimal-invalid-operation)))
    ()
  (let ((scale (%decimal-to-integer scale)))
    (unless (<= (abs scale) (* 2 (+ +maximum-exponent+ *precision*)))
      (decimal-error-cond (++qnan+ :return-p t)
        decimal-invalid-operation))
    (%scaleb-int x scale)))
