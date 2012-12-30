;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (c) 2009-2012 Gustavo Henrique Milaré
;;; See the file license for license information.

(in-package :cl-user)

(defpackage :dec-number-cffi
  (:use :cffi :alexandria cl)
  (:nicknames :dn-cffi)
  (:export #:single-get-exponent #:quad-copy-negate #:double-to-string
           #:double-same-quantum #:single-from-wider #:quad-log-b
           #:number-copy-sign #:double-add #:number-ln
           #:decimal-64-from-number #:double-is-subnormal #:quad-divide-integer
           #:quad-is-infinite #:number-and #:quad-min-mag
           #:number-copy-negate #:context-test-saved-status
           #:number-to-integral-value #:single-show #:quad-plus
           #:quad-from-bcd #:number-remainder #:quad-to-integral-exact
           #:double-is-logical #:double-from-u-int-32 #:double-remainder
           #:number-add #:decimal-64-canonical #:double-invert
           #:quad-remainder-near #:double-rotate #:number-power
           #:double-canonical #:decimal-64-to-string #:quad-show
           #:double-remainder-near #:number-to-packed #:number-copy
           #:double-is-positive #:number-is-subnormal #:number-multiply
           #:quad-or #:double-next-plus #:quad-copy
           #:quad-to-u-int-32 #:decimal-64-is-canonical #:decimal-32-from-string
           #:context-get-rounding #:number-compare-total-mag #:quad-scale-b
           #:quad-get-exponent #:quad-is-logical #:single-radix
           #:decimal-32-to-string #:double-quantize #:double-from-bcd
           #:double-divide #:quad-xor #:quad-set-coefficient
           #:single-get-coefficient #:quad-from-packed #:double-from-string
           #:number-class #:number-compare-signal #:number-same-quantum
           #:context-clear-status #:double-version #:single-to-packed
           #:number-or #:quad-to-packed #:double-class-string
           #:single-from-packed #:quad-min #:double-to-wider
           #:quad-to-bcd #:double-is-negative #:double-to-int-32
           #:double-show #:quad-from-string #:number-trim
           #:quad-fma #:number-next-toward #:double-copy-abs
           #:decimal-128-to-number #:number-plus #:number-compare-total
           #:double-is-canonical #:quad-invert #:number-reduce
           #:quad-rotate #:double-is-zero #:number-to-u-int-32
           #:double-to-bcd #:double-xor #:context-save-status
           #:quad-next-toward #:quad-next-plus #:single-set-exponent
           #:number-to-eng-string #:single-to-eng-string #:number-to-int-32
           #:double-copy-negate #:number-exp #:double-min
           #:quad-class #:single-zero #:single-to-wider
           #:single-to-string #:quad-copy-sign #:quad-divide
           #:context-default #:double-get-coefficient
           #:double-compare-total-mag #:double-max #:double-fma
           #:decimal-64-to-number #:quad-is-subnormal #:number-log-10
           #:quad-zero #:number-min-mag #:double-next-minus
           #:number-minus #:double-compare-total #:number-quantize
           #:double-compare #:number-next-plus #:double-is-signalling
           #:quad-is-na-n #:double-class #:decimal-32-from-number
           #:double-plus #:quad-compare-signal #:double-is-signaling
           #:single-version #:double-digits #:quad-same-quantum
           #:decimal-32-to-number #:single-from-string #:number-scale-b
           #:number-subtract #:context-restore-status #:quad-and
           #:quad-subtract #:decimal-128-is-canonical #:quad-multiply
           #:quad-from-int-32 #:double-from-packed #:context-set-rounding
           #:number-from-string #:context-set-status-quiet #:double-minus
           #:double-from-packed-checked #:quad-next-minus #:quad-add
           #:context-set-status-from-string #:quad-abs #:number-from-u-int-32
           #:double-to-u-int-32 #:double-to-integral-exact #:decimal-128-canonical
           #:number-rescale #:double-zero #:quad-is-canonical
           #:context-test-endian #:decimal-128-to-eng-string #:number-is-normal
           #:double-to-int-32-exact #:double-copy #:context-test-status
           #:quad-max #:quad-copy-abs #:number-version
           #:number-max-mag #:double-or #:single-set-coefficient
           #:quad-radix #:number-from-bcd #:quad-to-eng-string
           #:double-and #:double-compare-signal #:number-zero
           #:number-square-root #:number-class-to-string
           #:class #:decimal-64-to-eng-string #:number-shift
           #:decimal-128-from-string #:number-copy-abs #:quad-is-zero
           #:quad-digits #:double-is-integer #:double-is-na-n
           #:quad-class-string #:double-reduce #:double-abs
           #:quad-is-integer #:number-to-integral-exact #:double-next-toward
           #:number-to-string #:context-zero-status #:double-max-mag
           #:double-radix #:quad-version #:single-from-packed-checked
           #:number-abs #:double-get-exponent #:double-from-wider
           #:quad-compare-total-mag #:quad-is-finite #:number-from-int-32
           #:quad-from-u-int-32 #:double-from-int-32 #:quad-to-int-32
           #:context-set-status-from-string-quiet #:single-from-bcd
           #:quad-canonical #:quad-minus #:decimal-64-from-string
           #:decimal-64 #:number-xor #:double-is-infinite
           #:decimal-128-to-string #:number-next-minus #:quad-to-integral-value
           #:double-is-signed #:quad-set-exponent #:context-status-to-string
           #:quad-get-coefficient #:double-copy-sign #:quad-is-signed
           #:quad-is-positive #:number-invert #:number-rotate
           #:number-min #:context-get-status #:quad-quantize
           #:quad-from-packed-checked #:single-to-bcd #:single
           #:double-divide-integer #:number-to-bcd #:double-set-coefficient
           #:number-max #:quad-compare-total #:number-fma
           #:number-compare #:quad-shift #:quad-is-signalling
           #:double-to-u-int-32-exact #:quad-to-string #:double-min-mag
           #:quad-remainder #:context-set-status #:number-divide
           #:double-log-b #:quad-reduce #:quad-is-negative
           #:double-is-normal #:number-divide-integer #:double-subtract
           #:double-multiply #:quad-max-mag #:number-normalize
           #:decimal-32-is-canonical #:double-shift #:quad-is-signaling
           #:quad-compare #:double-to-eng-string #:quad-is-normal
           #:number-remainder-near #:quad-to-u-int-32-exact #:number-log-b
           #:decimal-128-from-number #:decimal-128 #:double-scale-b
           #:quad-to-int-32-exact #:quad #:double-to-packed
           #:double-set-exponent #:decimal-32-canonical #:double-is-finite
           #:decimal-32-to-eng-string #:decimal-32 #:number-from-packed
           #:lsu #:bits #:exponent #:number #:double-to-integral-value
           #:clamp #:status #:traps #:round #:emin #:emax #:digits
           #:number-error #:rounding #:words #:shorts #:bytes #:double
           #:context #:conditions #:default-context
           
           #:load-dec-number-library #:run-dec-make-number
           #:*dec-number-bin-dir* #:*dec-number-make-dir*
           #:*dec-number-make-environment*
           
           #:decnumber-unit #:+decnumber-unit-size+
           #:+decnumber-pmax+ #:+decnumber-emax+
           #:+decnumber-emin+ #:+decnumber-exponent-digits+
           #:+single-size+ #:+single-pmax+ #:+single-emin+
           #:+single-emax+ #:+single-string-size+
           #:+double-bytes+ #:+double-pmax+ #:+double-emin+
           #:+double-emax+ #:+double-string-size+
           #:+quad-bytes+ #:+quad-pmax+ #:+quad-emin+
           #:+quad-emax+ #:+quad-string-size+

           #:number-is-zero #:number-is-negative #:number-canonical
           #:number-class-string #:number-is-canonical #:number-is-finite
           #:number-is-infinite #:number-is-nan #:number-is-signed
           
           #:single-to-number #:single-from-number #:double-to-number
           #:double-from-number #:quad-to-number #:quad-from-number
           #:single-to-double #:single-to-quad #:double-to-single
           #:double-to-quad #:quad-to-single #:quad-to-double

           #:mem-ref-uint #:with-uint-pointer #:with-uint-pointers))
