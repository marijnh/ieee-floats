;;; Functions for converting floating point numbers represented in
;;; IEEE 754 style to lisp numbers.
;;;
;;; See http://common-lisp.net/project/ieee-floats/

(in-package :ieee-floats)

(define-condition conversion-error (error) ())
(define-condition underflow (conversion-error) ())
(define-condition overflow (conversion-error) ())

(defun make-converters-parts%
    (exponent-bits
     significand-bits
     support-nan-and-infinity-p
     &aux
       (total-bits (+ 1 exponent-bits significand-bits))
       (exponent-offset  (1- (expt 2 (1- exponent-bits))))
       (sign-part  `(ldb (byte 1 ,(1- total-bits)) bits))
       (exponent-part  `(ldb (byte ,exponent-bits ,significand-bits) bits))
       (significand-part  `(ldb (byte ,significand-bits 0) bits))
       (nan support-nan-and-infinity-p)
       (max-exponent (1- (expt 2 exponent-bits))))
  (values
   
   ;; ENCODER
   'float
   `(,@(unless nan `((declare (type float float))))
     (multiple-value-bind (sign significand exponent)
         (cond
           ,@(when nan `(((eq float :not-a-number)
                          (values 0 1 ,max-exponent))
                         ((eq float :positive-infinity)
                          (values 0 0 ,max-exponent))
                         ((eq float :negative-infinity)
                          (values 1 0 ,max-exponent))))
           (t
            (multiple-value-bind (significand exponent sign)
                (decode-float float)
              (let ((exponent (if (= 0 significand)
                                  exponent
                                  (+ (1- exponent) ,exponent-offset)))
                    (sign (if (= sign 1.0) 0 1)))
                (unless (< exponent ,(expt 2 exponent-bits))
                  (error "Floating point overflow when encoding ~A." float))
                (if (<= exponent 0)	; (C)
                    (values sign
                            (ash (round (* ,(expt 2 significand-bits)
                                           significand))
                                 exponent)
                            0)
                    (values sign
                            (round (* ,(expt 2 significand-bits)
                                      (1- (* significand 2))))
                            exponent))))))
       (let ((bits 0))
         (declare (type (unsigned-byte ,total-bits) bits))
         (setf ,sign-part sign
               ,exponent-part exponent
               ,significand-part significand)
         bits)))
   (format nil
           "Encode a float as an (UNSIGNED-BYTE ~A) integer.

The input variable ~A should be a Common Lisp float value
expressible as an IEEE-754 float with 1 bit for the sign, an
exponent of ~A bits and a significand part of ~A bits.~@[
Alternatively, the input can also be a keyword, namely
:NOT-A-NUMBER, :POSITIVE-INFINITY or :NEGATIVE-INFINITY.~]

The return value is an integer whose bits encode the nearest
float representing the input. If, however, the input float
value cannot be represented by the specified encoding an
error of type ~A is signaled."

           total-bits
           'float
           exponent-bits
           significand-bits
           support-nan-and-infinity-p
           (let ((*package* (find-package "KEYWORD")))
             (format nil "~:S" 'conversion-error)))
   
   ;; DECODER
   'bits
   (let ((block-name (gensym))
	 (float-category-expr `(cond
				 ((/= exponent ,max-exponent) nil)
				 ((not (zerop significand)) :not-a-number)
				 ((zerop sign) :positive-infinity)
				 (t :negative-infinity)))
	 (decode-float-body
           `((if (zerop exponent)       ; (D)
                 (setf exponent 1)
                 (setf (ldb (byte 1 ,significand-bits) significand) 1))
             (let ((float-significand (float
                                       significand
                                       ,(if (> total-bits 32) 1.0d0 1.0f0))))
               (scale-float (if (zerop sign)
                                float-significand
                                (- float-significand))
                            (- exponent ,(+ exponent-offset
                                            significand-bits)))))))
     `((declare (type (unsigned-byte ,total-bits) bits))
       
       (let* ((sign ,sign-part)
              (exponent ,exponent-part)
              (significand ,significand-part))
         ,@(case nan
             (:secondary
              `((let ((category ,float-category-expr))
                  ;; decode-float-body performs side effects, we
                  ;; must compute the category first.
                  (values (progn ,@decode-float-body)
                          category))))
             ((nil) decode-float-body)
             (t `((block ,block-name
                    ;; here we skip converting the value to a
                    ;; float if we detect it is a special float.
                    (when (= exponent ,max-exponent)
                      (return-from ,block-name
                        ,float-category-expr))
                    ,@decode-float-body)))))))
   (format nil
           "Decode an (UNSIGNED-BYTE ~A) as a float.

The input variable ~A should be an integer encoding a
floating-point value, with 1 bit for the sign, an exponent
of ~A bits and a significand part of ~A bits.~@[

This decoder supports Nan and Infinity values. If the input
variable encodes such special values, then the decoder can
also return :NOT-A-NUMBER, :POSITIVE-INFINITY
or :NEGATIVE-INFINITY ~[as a secondary value~;instead of a
float~].
~]"
           total-bits
           'bits
           exponent-bits
           significand-bits
           (and support-nan-and-infinity-p
                (case support-nan-and-infinity-p
                  (:secondary 0)
                  (t 1))))))

(defmacro with-converters-parts
    ((encoder-var encoder-body encoder-doc
      decoder-var decoder-body decoder-doc)
     (exponent-bits significand-bits support-nan-and-infinity-p)
     &body body)
  "<TODO>"
  `(multiple-value-bind (,encoder-var ,encoder-body ,encoder-doc
                         ,decoder-var ,decoder-body ,decoder-doc)
       (make-converters-parts% ,exponent-bits
                               ,significand-bits
                               ,support-nan-and-infinity-p)
     ,@body))

(defun assemble-converters-parts% (function-kind
                                   decoder-name
                                   encoder-name
                                   exponent-bits
                                   significand-bits
                                   support-nan-and-infinity-p)
  (labels
      ((assemble (evar ebody edoc dvar dbody ddoc)
         "Assemble the different parts of the converters.

EVAR and DVAR are symbols, free resp. in EBODY and DBODY.
EBODY is the code for the encoder, for an input float EVAR.
DBODY is the code for the decoder, for an input integer
DVAR.  EDOC and DDOC must be evaluated as strings and will
be used as documentation strings for resp. the encoder and
the decoder functions."
         (destructuring-bind (function-kind . local-body)
             (ensure-list function-kind)
           (ecase function-kind
             (:local
              (unless local-body (warn "Empty body"))
              `((flet ((,encoder-name (,evar) ,@ebody)
                       (,decoder-name (,dvar) ,@dbody))
                  (declare (ignorable (function ,encoder-name)
                                      (function ,decoder-name)))
                  (setf (documentation (function ,encoder-name) 'function) ,edoc)
                  (setf (documentation (function ,decoder-name) 'function) ,ddoc)
                  ,@local-body)))
             (:global
              `((defun ,encoder-name (,evar) ,@ebody)
                (setf (documentation ',encoder-name 'function) ,edoc)
                (defun ,decoder-name (,dvar) ,@dbody)
                (setf (documentation ',decoder-name 'function) ,ddoc)))))))
    (if (and (numberp exponent-bits)
             (numberp significand-bits)
             (symbolp support-nan-and-infinity-p))

        ;; constant parameters, compile immediately
        (with-converters-parts (evar ebody edoc dvar dbody ddoc)
            (exponent-bits significand-bits support-nan-and-infinity-p)
          (let ((code (assemble evar ebody edoc dvar dbody ddoc)))
            (if (rest code)
                `(progn ,@code)
                (first code))))

        ;; compilation at run-time, when parameters are known
        (with-gensyms (e d ev eb edoc ddoc dv db ev% dv%)
          `(with-converters-parts (,ev ,eb ,edoc ,dv ,db ,ddoc)
               (,exponent-bits ,significand-bits ,support-nan-and-infinity-p)
             (let ((,e (compile () `(lambda (,ev) ,@eb)))
                   (,d (compile () `(lambda (,dv) ,@deb))))
               ,@(assemble
                   ev% `((funcall ,e ,ev%)) edoc
                   dv% `((funcall ,d ,dv%)) ddoc)))))))

(eval-when (:compile-toplevel)
  (defun with-api (documentation)
    (concatenate 'string
                 documentation
                 "

If SUPPORT-NAN-AND-INFINITY-P is true, the decoder will
also understand these special cases and return symbols for
special floating point values: NaN is represented
as :NOT-A-NUMBER, and the infinities as :POSITIVE-INFINITY
and :NEGATIVE-INFINITY. In other words, it is not just
floating point numbers anymore, but also keywords.

If SUPPORT-NAN-AND-INFINITY-P is :SECONDARY, then the bits
are always decoded as a floating point value, and the
secondary value is either NIL or one of above symbols
representing special float values.

SUPPORT-NAN-AND-INFINITY-P different from NIL also means
that the encoding function accepts those symbols as
inputs. ")))

(defmacro with-float-converters ((decoder-name
				  encoder-name
				  exponent-bits
				  significand-bits
				  &optional (support-nan-and-infinity-p))
				 &body body)
  #.(with-api 
      "Temporarily bind DECODER-NAME and ENCODER-NAME to
floating point conversion functions with the given amount of
exponent and significand bits (plus an extra sign bit).")
  (assemble-converters-parts% `(:local ,@body)
                              decoder-name
                              encoder-name
                              exponent-bits
                              significand-bits
                              support-nan-and-infinity-p))

(defmacro make-float-converters (encoder-name
				 decoder-name
				 exponent-bits
				 significand-bits
				 support-nan-and-infinity-p)
  #.(with-api "Writes an encoder and decoder function for floating point
numbers with the given amount of exponent and significand
bits (plus an extra sign bit).") 
  (assemble-converters-parts% :global
                              decoder-name
                              encoder-name
                              exponent-bits
                              significand-bits
                              support-nan-and-infinity-p))

;; And instances of the above for the common forms of floats.

(declaim (inline encode-float32 decode-float32 encode-float64 decode-float64))
(make-float-converters encode-float32 decode-float32 8 23 nil)
(make-float-converters encode-float64 decode-float64 11 52 nil)

;;; Copyright (c) 2006 Marijn Haverbeke
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.
