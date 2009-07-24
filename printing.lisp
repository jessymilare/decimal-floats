;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(declaim (type function *printing-format*))

(def-customize-function (printing-format :scientific)
    (exponent adj-exponent digits)
  ;; Variable doc
  "The printing format that is used in case none has been specifyied.
 A valid printing format should be a function; use FIND-PRINTING-FORMAT
 to get most common printing formats.
 The method of PRINT-OBJECT for DECIMAL-FLOAT may ignore this value if
 *PRINT-READABLY* is true (some formats might break readability)."

  ;; Internalize doc
  "This function takes a keyword and returns the corresponding printing format function.
 Accepted keywords are :scientific, :engineering,
 :exponencial :signed-digits, :digits, :unsigned-digits,
 :signed-digits+exponent, :digits+exponent, and :unsigned-digits+exponent.

 Scientific format: http://speleotrove.com/decimal/daconvs.html#reftostr
 Engineering format: http://speleotrove.com/decimal/daconvs.html#reftoeng

 Information on how to create a printing format function is in the file \"doc/printing-format\".

 Please note that some formats may break readability (i.e. the number that is read back
 may not be the same). If readability is needed, use one of the following formats:
 :scientific, :engineering, :exponencial, :signed-digits+exponent and :digits+exponent"
  (declare (integer exponent adj-exponent) ; can't do any better
           (fixnum digits))
  (:scientific (if (and (<= exponent 0)
                        (<= -6 adj-exponent))
                   (values nil (if (zerop exponent)
                                   nil
                                   (+ digits exponent)))
                   (values adj-exponent (if (= 1 digits)
                                            nil
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
  (:exponencial (values adj-exponent 1))
  (:signed-digits (values nil nil t nil))
  (:digits (values nil nil nil nil))
  (:unsigned-digits (values nil nil nil t))
  (:signed-digits+exponent (values exponent nil t nil))
  (:digits+exponent (values exponent nil nil nil))
  (:unsigned-digits+exponent (values exponent nil nil t)))

(declaim (type function *printing-format*))

(defmethod print-object ((x decimal-float) stream)
  (write-string "#$" stream)
  (print-decimal x stream
      (if *print-readably*
          (get-printing-format :scientific)
          *printing-format*)))

(defun print-decimal (x stream &optional (format *printing-format* formatp) &aux place-plus-p omit-minus-p)
  "Prints the decimal X to STREAM. FORMAT should be either a keyword
 specifying a standard format or a function. The format argument
 only takes effect on finite numbers.

 The standard printing format functions are accessible through the
funtion GET-PRINTING-FORMAT."
  (if formatp
      (setf format (find-printing-format format)))
  (with-inf-nan-handler
      (x :inf-around (write-string (call-next-handler) stream)
         :inf-before (let (printed-exp dot-position place-dot-p) ; ignored variables
                       (multiple-value-setq (printed-exp dot-position place-plus-p omit-minus-p place-dot-p)
                         (funcall format 0 1 0)))
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
           (multiple-value-bind (printed-exp dot-position place-plus-p omit-minus-p place-dot-p)
               (funcall format 0 0 0)
             (declare (ignore printed-exp dot-position place-dot-p))
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
                 (array (%print-decimal stream (df-count-digits x) x-slots
                                        (df-first-slot-last-digit x)
                                        (df-last-slot-first-digit x)
                                        nil nil nil))
                 (condition             ; FIXME
                  ))))))
    (multiple-value-bind (digits exponent adj-exponent x-slots fsld lsfd)
        (parse-info x)
      (multiple-value-bind (printed-exp dot-position place-plus-p omit-minus-p place-dot-p)
          (funcall format exponent adj-exponent digits)
        ;; prints the negative sign, if any
        (cond
          ((df-negative-p x)
           (when (not omit-minus-p)
             (write-char #\- stream)))
          (place-plus-p
           (write-char #\+ stream)))
          ;; prints the rest
        (%print-decimal stream digits x-slots fsld lsfd printed-exp dot-position place-dot-p))))
  (force-output stream)
  x)

(defun %print-decimal (stream digits x-slots fsld lsfd printed-exp dot-position place-dot-p)
  (let* ((place-dot-p (and place-dot-p dot-position))
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
        (when place-dot-p
          (setf (char buffer pos) #\.)
          (incf pos)))
      (write-string buffer stream :end pos))
    ;; prints the exponent, if any
    (if printed-exp
        (format stream "E~10r" printed-exp))))

(set-dispatch-macro-character
 #\# #\$
 #'(lambda (stream char depth)
     (declare (ignore char depth))
     (let ((string (with-output-to-string (output)
                     (loop for char = (read-char stream nil #\  stream)
                        while (or (alphanumericp char)
                                  (member char '(#\+ #\- #\.)))
                        do (write-char char output)
                        finally (unread-char char stream)))))
       (parse-decimal-float string))))

(defun parse-decimal-float (string &key (start 0) end)
  (with-operation (parse-decimal-float condition (subseq string start (or end (length string))))
      (($conversion-syntax (make-qnan nil condition)))
    (let* ((end (or end (length string)))
           (position start)
           (signed-p (case (char string start)
                       (#\+ (incf position)
                            nil)
                       (#\- (incf position)
                            t)
                       (t nil))))
      (if (alpha-char-p (char string position))
          (progn
            (let* ((pos (mismatch "NaN" string :start2 position :end2 end
                                  :test #'char-equal))
                   (poss (mismatch "sNaN" string :start2 position :end2 end
                                   :test #'char-equal))
                   (signaling-p (when (or (not poss) (= poss 4))
                                  (setf pos poss)
                                  t)))
              (cond ((or (null pos) (= pos 3) signaling-p)
                     (let ((nan (if signaling-p
                                    (make-snan signed-p nil)
                                    (make-qnan signed-p nil))))
                       (when pos
                         (incf position (if signaling-p 4 3))
                         (when (position-if (lambda (x)
                                              (member x '(#\E #\e #\.)))
                                            string
                                            :start position :end end)
                           (calling-local-error #'LOCAL-ERROR nil $conversion-syntax))
                         (setf (values (df-slots nan)
                                       (df-first-slot-last-digit nan)
                                       (df-last-slot-first-digit nan))
                               ;; exponent and subnormal-p ignored
                               (%parse-decimal-float string position end #'LOCAL-ERROR)))
                       nan))
                    ((or (string-equal "Inf" string :start2 position :end2 end)
                         (string-equal "Infinity" string :start2 position :end2 end))
                     (make-infinity signed-p))
                    (t
                     (calling-local-error #'LOCAL-ERROR nil $conversion-syntax)))))
          (multiple-value-bind (slots fsld lsfd exponent subnormal-p)
              (%parse-decimal-float string position end #'local-error)
            (let ((x (%make-df slots exponent)))
              (setf (df-negative-p x) signed-p
                    (df-subnormal-p x) subnormal-p
                    (df-first-slot-last-digit x) fsld
                    (df-last-slot-first-digit x) lsfd)
              x))))))

(defun %parse-decimal-float (string start end LOCAL-ERROR)
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
                           (calling-local-error LOCAL-ERROR nil $conversion-syntax))
                         (handler-case (parse-integer string :start (1+ e-position) :end end)
                           (parse-error ()
                             (calling-local-error LOCAL-ERROR nil $conversion-syntax))))
                        (t (calling-local-error LOCAL-ERROR nil $conversion-syntax)))))
    (multiple-value-bind (slots fsld lsfd iexponent subnormal-digits)
        (calculate-info digits printed-exp dot-position)
      (let* ((position (- start subnormal-digits 1)))
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
                fsld lsfd iexponent (plusp subnormal-digits))))))
