;;; Functions for converting floating point numbers represented in
;;; IEEE 754 style to lisp numbers.
;;;
;;; See http://common-lisp.net/project/ieee-floats/

(in-package :ieee-floats)

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
  "Computes the different parts of the converter functions.

Given actual values for EXPONENT-BITS, SIGNIFICAND-BITS and
SUPPORT-NAN-AND-INFINITY-P, return 6 values that should be assembled by
macros to produce encoders and decoders for the specified format.

- ENCODER-VAR: the name of the free variable in ENCODER-BODY which
  represents the float value to be encoded.

- ENCODER-BODY: a list of forms (to be spliced) which returns the
  encoding of the float bound to ENCODER-VAR as bits in the IEEE-754
  format.

- ENCODER-DOCUMENTATION: the docstring attached to the encoding
  function.

- DECODER-VAR: the name of the free variable in DECODER-BODY which
  represents the bits to be decoded.

- DECODER-BODY: a list of forms (to be spliced) which returns the
  decoded float corresponding to the bits bound to DECODER-VAR.

- DECODER-DOCUMENTATION: the docstring attached to the decoding
  function.

- SIGN-PART
- EXPONENT-PART
- SIGNIFICAND-PART
- TOTAL-BITS
- MAX-EXPONENT
- EXPONENT-OFFSET"

  (check-type exponent-bits bit-length)
  (check-type significand-bits bit-length)
  (check-type support-nan-and-infinity-p nan-and-infinity-option)
  (values
   
   ;; ENCODER-VAR
   'float

   ;; ENCODER-BODY
   `((locally
         (declare
          (type ,(if nan '(or float (member
                                     :not-a-number
                                     :positive-infinity
                                     :negative-infinity))
                     'float)
                float))
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
                  (unless (< exponent ,max-exponent)
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
           bits))))
   
   ;; ENCODER-DOCUMENTATION
   (format nil
           "Encode a float as an (UNSIGNED-BYTE ~A) integer.

The input variable ~A is a Common Lisp float value, which is encoded
as an IEEE-754 float with 1 bit for the sign, an exponent of ~A bits
and a significand part of ~A bits.~@[~* Alternatively, the input can
also be a keyword: :NOT-A-NUMBER, :POSITIVE-INFINITY
or :NEGATIVE-INFINITY.~]

The return value is an integer whose bits encode the nearest float
representing the input. If, however, the input float value cannot be
encoded, an error of type ~A is signaled."
           total-bits
           'float
           exponent-bits
           significand-bits
           support-nan-and-infinity-p
           (let ((*package* (find-package "KEYWORD")))
             (format nil "~:S" 'conversion-error)))
   
   ;; DECODER-VAR
   'bits

   ;; DECODER-BODY
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
     `((locally
           (declare (type (unsigned-byte ,total-bits) bits))
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
                      ,@decode-float-body))))))))

   ;; DECODER-DOCUMENTATION
   (format nil
           "Decode an (UNSIGNED-BYTE ~A) as a Lisp float.

The input variable ~A should be an integer encoding a floating-point
value, with 1 bit for the sign, an exponent of ~A bits and a
significand part of ~A bits.~@[

This decoder supports NaN and Infinity values. If the input variable
encodes such special values, then the decoder can also
return :NOT-A-NUMBER, :POSITIVE-INFINITY or :NEGATIVE-INFINITY ~[as a
secondary value~;instead of a float~].~]

It is possible that the value being encoded cannot be represented as a
Lisp float, in which case the caller must be ready to handle underflow or
overflow errors from the implementation."
           total-bits
           'bits
           exponent-bits
           significand-bits
           (and support-nan-and-infinity-p
                (case support-nan-and-infinity-p
                  (:secondary 0)
                  (t 1))))

   ;; ADDITIONAL RETURN VALUES
   
   sign-part
   exponent-part
   significand-part
   total-bits
   max-exponent
   exponent-offset))

(defmacro with-converters-parts
    ((encoder-var encoder-body encoder-doc
      decoder-var decoder-body decoder-doc)
     (exponent-bits significand-bits support-nan-and-infinity-p)
     &body body)
  "Bind variables to the different parts of converter functions"
  `(multiple-value-bind (,encoder-var ,encoder-body ,encoder-doc
                         ,decoder-var ,decoder-body ,decoder-doc)
       (make-converters-parts% ,exponent-bits
                               ,significand-bits
                               ,support-nan-and-infinity-p)
     ,@body))

(defun assemble-converters-parts% (function-kind
                                   decoder-name
                                   encoder-name
                                   exponent-bits-form
                                   significand-bits-form
                                   support-nan-and-infinity-p-form)
  "Assemble code parts as local or global converter functions.

Associate DECODER-NAME and ENCODER-NAME with the converter functions
specified by EXPONENT-BITS-FORM, SIGNIFICAND-BITS-FORM and
SUPPORT-NAN-AND-INFINITY-P-FORM.

DECODER-NAME and ENCODER-NAME must be symbols.  EXPONENT-BITS-FORM,
SIGNIFICAND-BITS-FORM and SUPPORT-NAN-AND-INFINITY-P-FORM are expressions,
or literal values.

Converter functions are open-coded if both EXPONENT-BITS-FORM and
SIGNIFICAND-BITS-FORM are numbers and if SUPPORT-NAN-AND-INFINITY-P-FORM
is equal to one of T, NIL or :SECONDARY (any other symbol is assumed to be
a variable).  Otherwise, the code that is assembled will evaluate the
arguments and compile the actual functions at runtime. The
IEEE-FLOATS-UTILS package contains converter functions that work at
runtime without invoking the compiler.

FUNCTION-KIND can be either :GLOBAL or (:LOCAL . BODY), and respectively
corresponds to global functions (DEFUN) or local functions (FLET), where
BODY is the code to be executed while converter functions are locally
bound.
"
  (labels
      ((assemble (evar ebody edoc dvar dbody ddoc)
         (destructuring-bind (function-kind . local-body)
             (ensure-list function-kind)
           (ecase function-kind
             (:local
              (unless local-body (warn "Empty body"))
              `((flet ((,encoder-name (,evar) ,@ebody)
                       (,decoder-name (,dvar) ,@dbody))
                  (declare (ignorable #',encoder-name #',decoder-name))
                  (setf (documentation #',encoder-name 'function) ,edoc
                        (documentation #',decoder-name 'function) ,ddoc)
                  ,@local-body)))
             (:global
              `((defun ,encoder-name (,evar) ,@ebody)
                (setf (documentation ',encoder-name 'function) ,edoc)
                (defun ,decoder-name (,dvar) ,@dbody)
                (setf (documentation ',decoder-name 'function) ,ddoc)))))))
    
    (if (and (numberp exponent-bits-form)
             (numberp significand-bits-form)
             (typep support-nan-and-infinity-p-form 'nan-and-infinity-option))

        ;; literal parameters
        (with-converters-parts (evar ebody edoc dvar dbody ddoc)
            (exponent-bits-form
             significand-bits-form
             support-nan-and-infinity-p-form)
          (let ((code (assemble evar ebody edoc dvar dbody ddoc)))
            (if (rest code)
                `(progn ,@code)
                (first code))))

        ;; compilation at run-time, when parameters are known
        (with-gensyms (e d ev eb edoc ddoc dv db ev% dv%)
          `(with-converters-parts (,ev ,eb ,edoc ,dv ,db ,ddoc)
               (,exponent-bits-form
                ,significand-bits-form
                ,support-nan-and-infinity-p-form)
             (let ((,e (compile () `(lambda (,,ev) ,@,eb)))
                   (,d (compile () `(lambda (,,dv) ,@,db))))
               ,@(assemble
                   ev% `((funcall ,e ,ev%)) edoc
                   dv% `((funcall ,d ,dv%)) ddoc)))))))

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
