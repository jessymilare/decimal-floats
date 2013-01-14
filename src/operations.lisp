;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defun handle-snan (result)
  (decimal-error-cond
      ((cond
         ((df-slots result)
          (let ((result (copy-decimal result)))
            (setf (df-infinity-p result) nil)
            (multiple-value-bind (slots lsfd)
                (decapitate-nan-payload
                 (df-slots result)
                 (df-last-slot-first-digit result))
              (when slots
                (setf (df-slots result) slots
                      (df-last-slot-first-digit result) lsfd)))
            result))
         ((df-negative-p result) +-qnan+)
         (t ++qnan+)))
    decimal-invalid-operation))

(defmacro defop (name (&rest vars) (&key (return-first-qnan t) &allow-other-keys)
                 &body body)
  (with-gensyms (condition)
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
                       :nan
                       ;; Spec says to copy information
                       ;; from the first sNaN, if any, or
                       ;; of the first qNaN.
                       (or (and (df-infinity-p ,var)
                                (handle-snan ,var))
                           ,@(mapcar (lambda (varspec)
                                       (let ((var (car varspec)))
                                         `(and (df-not-a-number-p ,var)
                                               (df-infinity-p ,var)
                                               (handle-snan ,var))))
                                     tail)
                           ,(or (getf keys :nan) '(call-next-handler)))
                       :infinity
                       ;; We have to test if there is any NaN
                       ;; before calling the handler of this infinity.
                       (or ,@(maplist
                              (lambda (varspecs)
                                (let* ((var (caar varspecs)))
                                  `(and (df-not-a-number-p ,var)
                                        ;; Found a NaN; is there an sNaN?
                                        (or
                                         (and (df-infinity-p ,var)
                                              (handle-snan ,var))
                                         ,@(mapcar
                                            (lambda (varspec)
                                              (let ((var (car varspec)))
                                                `(and (df-not-a-number-p ,var)
                                                      (df-infinity-p ,var)
                                                      (handle-snan ,var))))
                                            (cdr varspecs))
                                         ;; There is no sNaN, only this NaN.
                                         ,(if return-first-qnan var)))))
                              tail)
                           ,(or (getf keys :infinity)
                                '(call-next-handler)))
                       ,@(progn
                          (remf keys :infinity)
                          (remf keys :nan)
                          keys))
                    ,body)))
      (deletef vars '&others)
      `(defun ,name ,(mapcar #'ensure-car vars)
         ,@(ensure-list documentation)
         ,@declarations
         (with-operation (,name ,@(mapcar #'ensure-car vars))
           ,body)))))

(declaim (inline logb-int))
(defop logb-int ((x :infinity ++infinity+))
    ()
  (if (df-zero-p x)
      (decimal-error-cond (+-infinity+)
        decimal-division-by-zero)
      (df-logb x)))

(declaim (inline logb))
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

(declaim (inline scaleb-int))
(defop scaleb-int ((x :infinity x) &others scale)
    ()
  (declare (type integer scale))
  (unless (<= (abs scale) (* 2 (+ +maximum-exponent+ *precision*)))
    (decimal-error-cond (++qnan+ :return-p t)
      decimal-invalid-operation))
  (%scaleb-int x scale))

(declaim (inline scaleb))
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

(defun %compare-total-mag (x y)
  ;; Returns:
  ;; -2 if X is numerically smaller than Y,
  ;; -1 if X and Y are numerically equal but X is lower than Y in total ordering,
  ;;  0 if they are exactly equal,
  ;; +1 if X and Y are numerically equal but X is higher than Y in total ordering,
  ;; +2 if X is numerically larger than Y.
  (let* ((x-iexp (df-iexponent x))
         (y-iexp (df-iexponent y))
         (x-slots (df-slots x))
         (y-slots (df-slots y))
         (x-length (length x-slots))
         (y-length (length y-slots)))
    (cond
      ((%df-zero-p x-iexp x-slots x-length)
       ;; X is zero. Is Y also zero?
       (if (%df-zero-p y-iexp y-slots y-length)
           ;; X and Y are both zeros, so the higher one
           ;; is the one with greater exponent.
           (cond
             ((> x-iexp y-iexp) +1)
             ((< x-iexp y-iexp) -1)
             (t ; (= x-iexp y-iexp)
              (let ((x-lsfd (df-last-slot-first-digit x))
                    (y-lsfd (df-last-slot-first-digit y)))
                (cond
                  ((> x-lsfd y-lsfd) +1)
                  ((< x-lsfd y-lsfd) -1)
                  (t 0)))))
           ;; X is zero, Y isn't...
           -2))
      ((> x-iexp y-iexp)
       ;; X has a greater exponent and is not zero.
       +2)
      ((< x-iexp y-iexp)
       ;; Y might still be zero.
       (if (zerop (aref y-slots (1- y-length)))
           +2
           -2))
      (t ; (= x-iexp y-iexp)
       (or (loop
              for i from (1- x-length) downto 0
              and j from (1- y-length) downto 0
                for x-slot = (aref x-slots i)
                and y-slot = (aref y-slots j)
              do
                (cond
                  ((> x-slot y-slot) (return +2))
                  ((< x-slot y-slot) (return -2))))
           (cond
             ((> x-length y-length)
              ;; X has more digits than Y.
              ;; Is some of the extra digits non-zero?
              (if (find-if #'plusp x-slots :end (- x-length y-length))
                  ;; X is numerically larger.
                  +2
                  ;; X and Y are numerically equal and X has smaller exponent.
                  -1))
             ((< x-length y-length)
              ;; Y has more digits then X.
              ;; Is some of the extra digits non-zero?
              (if (find-if #'plusp y-slots :end (- y-length x-length))
                  ;; Y is numerically larger
                  -2
                  ;; X and Y are numerically equal and X has greater exponent.
                  +1))
             (t ; (= x-length y-length)
              (let ((x-fsld (df-first-slot-last-digit x))
                    (y-fsld (df-first-slot-last-digit y)))
                (cond
                  ((< x-fsld y-fsld) -1)
                  ((> x-fsld y-fsld) +1)
                  (t 0))))))))))

(declaim (inline maximum-abs))
(defop maximum-abs ((x :+infinity x
                       :-infinity (if (and (df-infinity-p y)
                                           (not (df-negative-p y)))
                                      y
                                      x)
                       :qnan (if (df-not-a-number-p y) x y))
                    (y :infinity y
                       :qnan x))
    (:return-first-qnan nil)
  (let ((compare (%compare-total-mag x y)))
    (%round-decimal (case compare
                      (-2 y)
                      (+2 x)
                      (-1 (if (df-negative-p y)
                              x
                              y))
                      (+1 (if (df-negative-p x)
                              y
                              x))
                      (t  (if (df-negative-p y)
                              x
                              y))))))

(declaim (inline maximum))
(defop maximum ((x :+infinity x
                   :-infinity (if (df-not-a-number-p y) x y)
                   :qnan (if (df-not-a-number-p y) x y))
                (y :+infinity y
                   :-infinity x
                   :qnan x))
    (:return-first-qnan nil)
  (%round-decimal
   (if (df-negative-p x)
       (if (df-negative-p y)
           (if (<= (%compare-total-mag x y) 0) x y)
           y)
       (if (df-negative-p y)
           x
           (if (>= (%compare-total-mag x y) 0) x y)))))

(declaim (inline minimum-abs))
(defop minimum-abs ((x :infinity (if (df-not-a-number-p y)
                                     x
                                     (call-next-handler))
                       :+infinity y
                       :-infinity (if (and (df-infinity-p y)
                                           (not (df-negative-p y)))
                                      x
                                      y)
                       :qnan (if (df-not-a-number-p y) x y))
                    (y :infinity x
                       :qnan x))
    (:return-first-qnan nil)
  (let ((compare (%compare-total-mag x y)))
    (%round-decimal (case compare
                      (-2 x)
                      (+2 y)
                      (-1 (if (df-negative-p y)
                              y
                              x))
                      (+1 (if (df-negative-p x)
                              x
                              y))
                      (t  (if (df-negative-p y)
                              y
                              x))))))

(declaim (inline minimum))
(defop minimum ((x :+infinity (if (df-not-a-number-p y) x y)
                   :-infinity x
                   :qnan (if (df-not-a-number-p y) x y))
                (y :+infinity x
                   :-infinity y
                   :qnan x))
    (:return-first-qnan nil)
  (%round-decimal
   (if (df-negative-p x)
       (if (df-negative-p y)
           (if (>= (%compare-total-mag x y) 0) x y)
           x)
       (if (df-negative-p y)
           y
           (if (<= (%compare-total-mag x y) 0) x y)))))

(declaim (inline dec-abs))
(defop dec-abs ((x :infinity ++infinity+))
    ()
  (%round-decimal (copy-abs x)))

(declaim (inline plus))
(defop plus ((x :infinity x))
    ()
  (%round-decimal (if (and (df-negative-p x)
                           (df-zero-p x))
                      (copy-minus x)
                      x)))

(declaim (inline minus))
(defop minus ((x :+infinity +-infinity+
                 :-infinity ++infinity+))
    ()
  (%round-decimal (if (and (not (df-negative-p x))
                           (df-zero-p x))
                      x
                      (copy-minus x))))
