;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package :decimal-floats)

(def-customize-function $get-printing-format (exponent adj-exponent digits)
  "See documentation of PRINT-DECIMAL."
  (:scientific (if (and (<= exponent 0)
                        (<= -6 adj-exponent))
                   (values nil (let ((pos (+ digits exponent)))
                                 (unless (= pos digits)
                                   pos)))
                   (values adj-exponent (unless (= 1 digits)
                                          1))))
  (:engineering (if (and (<= exponent 0)
                         (<= -6 adj-exponent))
                    (values nil (+ digits exponent))
                    (let* ((adjust (mod adj-exponent 3)))
                      (decf adj-exponent adjust)
                      (values (if (zerop adj-exponent)
                                  nil
                                  adj-exponent)
                              (+ 1 adjust)))))
  (:lispy (if (and (<= exponent 0)
                   (<= -6 adj-exponent))
              (values nil (+ digits exponent))
              (values adj-exponent 1)))
  (:exponencial (values adj-exponent (unless (= 1 digits)
                                       1)))
  (:signed-digits (values nil nil t nil))
  (:digits nil)
  (:unsigned-digits (values nil nil nil t))
  (:signed-digits+exponent (values exponent nil nil t nil))
  (:digits+exponent exponent)
  (:unsigned-digits+exponent (values exponent nil nil nil t)))

(defvar *decimal-printing-format* ($get-printing-format :scientific)
  "The standard format that is used in case none is specifyied.
 The method of PRINT-OBJECT for decimal-float may ignore this value if
 *print-readably* is true and the format is not one of the readable standard
 formats (other formats might break readability).")

(defmethod print-object ((x decimal-float) stream)
  (write-string "#$" stream)
  (print-decimal x stream
      (if *print-readably*
          :scientific
          *decimal-printing-format*)))

(defun print-decimal (x stream &optional (format *decimal-printing-format*) &aux place-plus-p omit-minus-p)
  "Prints the decimal X to STREAM. FORMAT should be either a keyword
 specifying a standard format or a function. The format argument
 only takes effect on finite numbers.

 Accepted keywords are :scientific, :engineering, :lispy,
 :exponencial :signed-digits, :digits, :unsigned-digits,
 :signed-digits+exponent, :digits+exponent, and :unsigned-digits+exponent,

 If it is a function, it should take three arguments:

  - EXPONENT, the exponent of the number, i.e., the exponent to be used
    if all digits are printed as an integer.
    (In particular, if EXPONENT is non-negative, the number is integral).
    It will be 0 if number is special (i.e. infinity or NaN)

  - ADJUSTED-EXPONENT, the adjusted exponent of the number, i.e., the
    exponent to be used if only one digit is placed before the point.
    It will be 0 if number is a NaN or 1 if number is infinity.

  - DIGITS, the number of significant digits to be printed
    (including zeroes to the right). It will be 1 if the number is zero,
    and 0 if (and only if) the number is special.

 They have the mathematical relation

   EXPONENT + (DIGITS - 1) = ADJUSTED-EXPONENT.

 This function must return these values, in order (non-returned values are always nil):

  - PRINTED-EXPONENT, the exponent to be printed (after the 'E' character)
    or NIL not to use scientific notation.
    This is ignored if the number to be printed is special.

  - DOT-POSITION, the position of the point ('.').
    If positive, it tells how many digits will be printed before the point,
    including zeroes if necessary (a zero is added after the dot as well if necessary).
    If it is zero, a single zero digit will be placed before the point.
    If it is negative, a zero digit is placed, then the decimal point, then
    (- DOT-POSITION) zero digits are placed before the digits of the number.
    If it is nil, the point is omitted.
    This is ignored if the number to be printed is special.

  - PLACE-PLUS-P, tells if the sign '+' must be *placed* if the number is positive
    (or a positive zero).

  - OMIT-MINUS-P, tells if the minus sign '-' must be *omitted* if the number is
    negative (or a negative zero).

 If the printed number must represent the actual number, the relation

   (DOT-POSITION or 0) - 1 + PRINTED-EXPONENT = ADJUSTED-EXPONENT

should be respected, but disrespecting it is valid if desired
 (see, for instance, the :digits format).
Also, if the precision must be preserved, then DOT-POSITION must either
 strictly less than DIGITS or nil.
These relations are satisfyied only by :scientific, :engineering,
 :exponencial, :signed-digits+exponent and :digits+exponent
 among the provided formats.

 The format function corresponding to the keywords are accessible through the
funtion $get-printing-format."
  (with-inf-nan-handler
      (x :inf-around (write-string (call-next-handler) stream)
         :inf-before (let (printed-exp dot-position) ; ignored variables
                       (multiple-value-setq (printed-exp dot-position place-plus-p omit-minus-p)
                         (call-customize-function #'$get-printing-format format 0 1 0)))
         :+infinity (if place-plus-p
                        "+Infinity"
                        "Infinity")
         :-infinity (if omit-minus-p
                        "Infinity"
                        "-Infinity")
         :qnan "NaN"
         :snan "sNaN"
         :nan-around
         (progn
           (multiple-value-bind (printed-exp dot-position place-plus-p omit-minus-p)
               (call-customize-function #'$get-printing-format format 0 0 0)
             (declare (ignore printed-exp dot-position))
             ;; prints the negative sign, if any
             (cond
               ((df-negative-p x)
                (when (not omit-minus-p)
                  (write-char #\- stream)))
               (place-plus-p
                (write-char #\+ stream)))
             ;; prints "NaN" or "sNaN", according to NaN type
             (write-string (call-next-handler) stream)
             ;; prints NaN diagnostic number, if any
             (when-let ((x-slots (df-slots x)))
               (etypecase x-slots
                 (array (%print-decimal stream (%count-digits x) x-slots
                                        (df-first-slot-last-digit x)
                                        (df-last-slot-first-digit x)
                                        nil nil))
                 (condition             ; FIXME
                  ))))))
    (multiple-value-bind (digits exponent adj-exponent x-slots fsld lsfd)
        (parse-info x)
      (multiple-value-bind (printed-exp dot-position place-plus-p omit-minus-p)
            (call-customize-function #'$get-printing-format format exponent adj-exponent digits)
          ;; prints the negative sign, if any
          (cond
            ((df-negative-p x)
             (when (not omit-minus-p)
               (write-char #\- stream)))
            (place-plus-p
             (write-char #\+ stream)))
          ;; prints the rest
          (%print-decimal stream digits x-slots fsld lsfd printed-exp dot-position))))
  (force-output stream)
  x)

(defun %print-decimal (stream digits x-slots fsld lsfd printed-exp dot-position)
  (let* ((insert-dot-zero-p dot-position)
         (dot-position (or dot-position digits))
         (buffer (make-string (max (1+ digits) (if (minusp dot-position)
                                                   (- 2 dot-position)
                                                   dot-position)
                                   (1+ +decimal-slot-digits+))
                             :element-type 'base-char :initial-element #\0)))
    ;; prints zeroes up to the digits of the number, if any
    (cond
      ((= dot-position 0) (write-char #\0 stream))
      ((< dot-position 0)
       (setf (char buffer 1) #\.)
       (write-string buffer stream :end (- 2 dot-position))))
    ;; prints the digits of the number
    (let ((pos 0))
      (map-digits-array #'(lambda (digit)
                            (when (= dot-position pos)
                              (setf (char buffer pos) #\.)
                              (incf pos))
                            (setf (char buffer pos)
                                  (digit-char digit))
                            (incf pos))
                        x-slots fsld lsfd nil)
      (when (>= dot-position pos)
        (when (> dot-position pos)
          (fill buffer #\0 :start pos :end dot-position)
          (setf pos dot-position))
        (when insert-dot-zero-p
          (setf (subseq buffer dot-position (1+ dot-position)) ".0")
          (incf pos 2)))
      (write-string buffer stream :end pos))
    ;; prints the exponent, if any
    (if printed-exp
        (format stream "E~10r" printed-exp))))

#+nil
(set-dispatch-macro-character
 #\# #$
 #'(lambda (stream char depth)
     (declare (ignore char depth))
     (let ((string (with-output-to-string (output)
                     (loop for char = (read-char stream nil #\  stream)
                        while (or (alphanumericp char)
                                  (member char '(#\+ #\- #\.)))
                        do (write-char char output)
                        finally (unread-char char stream)))))
       (parse-decimal-float string))))

(define-condition decimal-float-parse-error (parse-error)
  ((string :accessor df-parse-error-string :initarg :string))
  (:report (lambda (condition stream)
             (format stream "Invalid decimal float string: \"~a\"."
                     (df-parse-error-string condition)))))

(defun parse-decimal-float (string &key (start 0) end)
  (handler-case
      (let* ((end (or end (length string)))
             (position start)
             (signed-p (case (char string start)
                         (#\+ (incf position)
                              nil)
                         (#\- (incf position)
                              t)
                         (t nil))))
        (if (alpha-char-p (char string start))
            (progn
              (let* ((pos (mismatch "NaN" string :start2 start :end2 end
                                    :test #'char-equal))
                     (poss (mismatch "qNaN" string :start2 start :end2 end
                                     :test #'char-equal))
                     (signaling-p (when (or (not poss) (= poss 4))
                                    (setf pos poss)
                                    t)))
                (cond ((or (null pos) (= pos 3) signaling-p)
                       (let ((nan (if signaling-p
                                      (make-qnan signed-p nil)
                                      (make-snan signed-p nil))))
                         (when pos
                           (incf position (if signaling-p 4 3))
                           (when (position-if (lambda (x)
                                                (member x '(#\E #\e #\.)))
                                              string
                                              :start position :end end)
                             (error 'decimal-float-parse-error))
                           (setf (values (df-slots nan)
                                         (df-first-slot-last-digit nan)
                                         (df-last-slot-first-digit nan))
                                 ;; exponent and subnormal-p ignored
                                 (%parse-decimal-float string position end)))
                         nan))
                      ((or (string-equal "Inf" string :start2 position :end2 end)
                           (string-equal "Infinite" string :start2 position :end2 end))
                       (make-infinity signed-p))
                      (t
                       ;; ERROR
                       (error 'decimal-float-parse-error)))))
            (multiple-value-bind (slots fsld lsfd exponent subnormal-p)
                (%parse-decimal-float string position end)
              (let ((x (%make-df slots exponent)))
                (setf (df-negative-p x) signed-p
                      (df-subnormal-p x) subnormal-p
                      (df-first-slot-last-digit x) fsld
                      (df-last-slot-first-digit x) lsfd)
                x))))
    (decimal-float-parse-error ()
      (let ((condition (make-condition 'decimal-float-parse-error
                                       :string (subseq string start end))))
        (cerror "Return NaN" condition)
        (return-from parse-decimal-float (make-qnan nil condition))))))

(defun %parse-decimal-float (string start end)
  (let* ((e-position (or (position #\e string :start start :end end :test #'char-equal)
                         end))
         (dot-position (or (position #\. string :start start :end e-position :test #'char=)
                           e-position))
         (digits (- e-position start
                    (if (< dot-position e-position) 1 0)))
         (printed-exp (cond
                        ((= e-position end) 0)
                        ((< e-position (1- end))
                         (unless (and (let ((char (char string (1+ e-position))))
                                        (or (digit-char-p char)
                                            (find char "+-")))
                                      (digit-char-p (char string (1- end))))
                           ;; parse-integer accepts leading and trailing whitespaces, but spec doesn't.
                           (error 'decimal-float-parse-error))
                         (handler-case (parse-integer string :start (1+ e-position) :end end)
                           (parse-error ()
                             (error 'decimal-float-parse-error))))
                        (t (error 'decimal-float-parse-error)))))
    (multiple-value-bind (slots fsld lsfd df-exponent subnormal-digits)
        (calculate-info digits printed-exp dot-position)
      (let ((position (- start subnormal-digits 1)))
        (values (map-digits-array
                 #'(lambda (old-digit)
                     (declare (ignore old-digit))
                     (incf position)
                     (if (= position dot-position)
                         (incf position))
                     (if (< position start) ; subnormal-digit
                         0
                         (digit-char-p (char string position))))
                 slots fsld lsfd t)
                fsld lsfd df-exponent (plusp subnormal-digits))))))
