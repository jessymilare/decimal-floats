;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :decimal-floats)

(defun digits-to-dpd (d2 d1 d0)
  (declare (type (integer 0 10) d0 d1 d2))
  (if (< d2 8)
      (if (< d1 8)
          ;; All digits are small or right digit is large
          (logior (ash d2 7) (ash d1 4) d0)
          (if (< d0 8)
              ;; Middle digit is large
              (logior (ash d2 7)
                      (ash (logior (logand #b0001 d1)
                                   (logand #b0110 d0))
                           4)
                      #b1010
                      (logand d0 #b0001))
              ;; Left digit is small
              (logior (ash d2 7)
                      (ash (logand #b0001 d1) 4)
                      #b1001110
                      (logand d0 #b0001))))
      (if (< d1 8)
          (if (< d0 8)
              ;; Left digit is large
              (logior (ash (logior (logand #b0001 d2)
                                   (logand #b0110 d0)) 7)
                      (ash d1 4)
                      #b1100
                      (logand d0 #b0001))
              ;; Middle digit is small
              (logior (ash (logior (logand #b0001 d2)
                                   (logand #b0110 d1)) 7)
                      (ash (logand #b0001 d1) 4)
                      #b0101110
                      (logand d0 #b0001)))
          (if (< d0 8)
              ;; Right digit is small
              (logior (ash (logior (logand #b0001 d2)
                                   (logand #b0110 d0)) 7)
                      (ash (logand #b0001 d1) 4)
                      #b1110
                      (logand d0 #b0001))
              ;; All digits are large
              (logior (ash (logand #b0001 d2) 7)
                      (ash (logand #b0001 d1) 4)
                      (logand #b0001 d0)
                      #b1101110)))))

(defun dpd-to-digits (dpd)
  (declare (type (unsigned-byte 10) dpd))
  (if (logbitp 3 dpd) ; v
      (if (logbitp 2 dpd) ; w
          (if (logbitp 1 dpd) ; x
              (if (logbitp 6 dpd) ; s
                  (if (logbitp 5 dpd) ; t
                      ;; All digits are large
                      (values (+ 8 (ldb (byte 1 0) dpd))
                              (+ 8 (ldb (byte 1 4) dpd))
                              (+ 8 (ldb (byte 1 7) dpd)))
                      ;; Left digit is small
                      (values (+ 8 (ldb (byte 1 0) dpd))
                              (+ 8 (ldb (byte 1 4) dpd))
                              (ldb (byte 3 7) dpd)))
                  (if (logbitp 5 dpd) ; t
                      ;; Middle digit is small
                      (values (+ 8 (ldb (byte 1 0) dpd))
                              (+ (ldb (byte 1 4) dpd)
                                 (ash (ldb (byte 2 8) dpd)
                                      1))
                              (+ 8 (ldb (byte 1 7) dpd)))
                      ;; Right digit is small
                      (values (+ (ldb (byte 1 0) dpd)
                                 (ash (ldb (byte 2 8) dpd)
                                      1))
                              (+ 8 (ldb (byte 1 4) dpd))
                              (+ 8 (ldb (byte 1 7) dpd)))))
              ;; Left digit is large
              (values (+ (ldb (byte 1 0) dpd)
                         (ash (ldb (byte 2 8) dpd)
                              1))
                      (ldb (byte 3 4) dpd)
                      (+ 8 (ldb (byte 1 7) dpd))))
          (if (logbitp 1 dpd) ; x
              ;; Middle digit is large
              (values (+ (ldb (byte 1 0) dpd)
                         (ash (ldb (byte 2 5) dpd)
                              1))
                      (+ 8 (ldb (byte 1 4) dpd))
                      (ldb (byte 3 7) dpd))
              ;; Right digit is large
              (values (ldb (byte 4 0) dpd)
                      (ldb (byte 3 4) dpd)
                      (ldb (byte 3 7) dpd))))
      ;; All digits are small
      (values (ldb (byte 3 0) dpd)
              (ldb (byte 3 4) dpd)
              (ldb (byte 3 7) dpd))))


(defun calculate-offsets (number-of-bits)
  (declare (type fixnum number-of-bits))
  (multiple-value-bind (4B-groups rem) (truncate number-of-bits 32)
    (assert (zerop rem))
    (let* ((ecbits (+ 4 (* 2 4B-groups)))
           (ccbits (+ -10 (* 30 4B-groups)))
           (precision (+ -2 (* 9 4B-groups)))
           (Emax (* 3 (ash 1 (1- ecbits))))
           (Emin (- 1 Emax))
           (bias (+ Emax precision -2)))
      (values ccbits (+ ccbits ecbits) (- number-of-bits 6)
              precision Emax Emin bias))))

(defun parse-combination-field (field)
  (declare (type (unsigned-byte 5) field))
  (let ((ab (ldb (byte 2 3) field)))
    (if (= #b11 ab)
        (let ((cd (ldb (byte 2 1) field)))
          (if (= #b11 cd)
              (if (logbitp 0 field)
                  (values 0 0 :infinity)
                  (values 0 0 :nan))
              ;; Finite number, MSD >= 8
              (values cd (logior #b1000
                                 (logand #b0001 field)))))
        ;; Finite number, MSD < 8
        (values ab (ldb (byte 3 0) field)))))

(defun create-combination-field (coef-msd exp-msb inf-or-nan-p)
  (case inf-or-nan-p
    (:nan #b11111)
    (:infinity #b11110)
    (t (if (< 8 coef-msd)
           (dpb exp-msb (byte 2 3)
                coef-msd)
           (dpb exp-msb (byte 2 1)
                (logior #b11000
                        (logand #b0001 coef-msd)))))))
