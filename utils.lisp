(in-package :ieee-floats)

;; TODO Use float-radix (usually 2)
;; TODO Use float-precision (usually 24)?
;; TODO Can we use integer-decode-float instead?

(defun encode
    (float exponent-bits significand-bits
     &aux
       (total-bits (+ 1 exponent-bits significand-bits))
       (exponent-offset (1- (expt 2 (1- exponent-bits))))
       (max-significand (expt 2 significand-bits))
       (extra-significand-bits (1+ significand-bits))
       (max-exponent (1- (expt 2 exponent-bits))))
  "Generic encoding function"
  (declare
   (type (or float
             (member :not-a-number
                     :positive-infinity
                     :negative-infinity)) float))
  (flet ((encode (sign significand exponent remainder &aux (bits 0))
           (declare (type unsigned-byte bits))
           (setf (ldb (byte 1 (1- total-bits)) bits) sign
                 (ldb (byte exponent-bits significand-bits) bits) exponent
                 (ldb (byte significand-bits 0) bits) significand)
           (values bits (typecase remainder
                          (symbol remainder)
                          (number (case sign
                                    (0 remainder)
                                    (1 (- remainder))))))))
    (case float
      (:not-a-number      (encode 0 1 max-exponent 0.0))
      (:positive-infinity (encode 0 0 max-exponent 0.0))
      (:negative-infinity (encode 1 0 max-exponent 0.0))
      (t (multiple-value-bind (original-significand exponent sign)
             (decode-float float)

           ;; Adjust sign
           (setf sign (if (= sign 1.0) 0 1))

           (if (zerop original-significand)
               (encode sign 0 0 0)
               (let ((biased-exponent (+ exponent exponent-offset -1)))
                 (macrolet
                     ((check-overflow ()
                        ;; >= means NaN too are considered
                        ;; > overflow.  means that we accept
                        ;; > values which encode NaN.
                        '(when (>= biased-exponent max-exponent)
                          (return-from encode
                            (encode sign 0 max-exponent :overflow)))))
                   (check-overflow)
                   (multiple-value-bind (significand remainder)
                       (if (plusp biased-exponent)
                           (multiple-value-bind (sig rem)
                               (round (- (scale-float original-significand
                                                      extra-significand-bits)
                                         max-significand))
                             (values sig (scale-float
                                          rem (- exponent
                                                 extra-significand-bits))))
                           (multiple-value-bind (sig rem)
                               (round (scale-float original-significand
                                                   (+ significand-bits
                                                      biased-exponent)))
                             (values sig
                                     (scale-float rem (- 1
                                                         exponent-offset
                                                         significand-bits)))))
                     
                     ;; ROUND can lead to MAX-SIGNIFICAND, which
                     ;; would reset all bits to zero (loss of
                     ;; accuracy); instead, we go up to the next
                     ;; representable float.

                     (when (= significand max-significand)
                       (incf biased-exponent)
                       (setf significand 0))

                     (check-overflow)

                     (encode sign
                             significand
                             (max 0 biased-exponent)
                             remainder))))))))))

(defun float-dice (exponent)
  (let ((offset (- (expt 2 (1- exponent))))
        (width (float (expt 2 exponent))))
    (lambda ()
      (+ (random width) offset))))

(defparameter *check-nearest-encoding* t)

(defun back-and-forth-p (float encoder decoder exponent-bits significand-bits)
  (multiple-value-bind (bits difference)
      (funcall encoder float)
    (let* ((total-bits (+ 1 exponent-bits significand-bits))
           (decoded (funcall decoder bits))
           (up (funcall decoder (mod (1+ bits) total-bits)))
           (down (funcall decoder (mod (1- bits) total-bits)))
           (adjusted (+ decoded difference)))
      (values  
       (case difference
         (:overflow t)
         (t (restart-case
                (prog1 T
                  (assert (= float adjusted))
                  (when *check-nearest-encoding*
                    (assert (>= (abs (- up float)) difference) (up))
                    (assert (>= (abs (- float down)) difference) (down))))
              (ignore () :report "Ignore this assertion" nil))))
       float
       bits
       decoded
       difference
       adjusted))))

(defun make-back-and-forth-p (exponent-bits significand-bits encoder)
  (with-float-converters (_ dec exponent-bits significand-bits nil)
    (lambda (float)
      (back-and-forth-p float encoder #'dec exponent-bits significand-bits))))

(defun make-encoder-closure (exponent-bits significand-bits)
  (lambda (float)
    (encode float exponent-bits significand-bits)))

(defun make-encoder-closure-for-original (original-encoder original-decoder)
  (lambda (float)
    (let* ((encoded (funcall original-encoder float))
           (decoded (funcall original-decoder encoded)))
      (values encoded (- float decoded)))))

(defun back-and-forth (times exp sig encoder)
  (loop
    with dice = (float-dice exp)
    with test = (make-back-and-forth-p exp sig encoder)
    repeat times
    for float = (funcall dice)
    for success = (restart-case (funcall test float)
                    (stop-loop () (loop-finish)))
    unless success
      collect float))


(define-symbol-macro $
    (back-and-forth 10000000 4 4 (make-encoder-closure 4 4)))



(make-float-converters encode-float9 decode-float9 4 4 nil)


(let ((*check-nearest-encoding* nil))
  (back-and-forth 1000000 4 4 (make-encoder-closure-for-original
                               #'encode-float9
                               #'decode-float9)))

(let ((*check-nearest-encoding* t))
  (let ((counter 100))
    (handler-bind ((simple-error
                     (lambda (e)
                       (unless (plusp (decf counter))
                         (invoke-restart 'stop-loop))
                       (invoke-restart 'ignore))))
      (back-and-forth 1000000 4 4 (make-encoder-closure-for-original
                                   #'encode-float9
                                   #'decode-float9)))))

;; (8.6021423e-4 0.0036010742 0.002796173 0.004436493 0.0034637451 0.004453659
;;  0.0034637451 0.004486084 0.0035629272 0.015457153 0.015623093 0.0055770874
;;  0.0037574768 0.002527237 0.015363693 0.0155239105 0.0018692017 0.01533699
;;  0.0045433044 0.0065784454 5.760193e-4 0.0024681091 0.0015735626 0.004419327
;;  0.015584946 0.002603531 0.0045280457 0.0035076141 0.015417099 0.005470276
;;  0.002790451 0.003440857 0.0026950836 0.005525589 0.0026664734 0.0054092407
;;  0.015407562 6.2179565e-4 0.0054779053 0.015417099 0.005437851 0.0017356873
;;  0.0065841675 0.00551033 5.5503845e-4 0.0044994354 0.004535675 5.893707e-4
;;  0.0014705658 0.006450653 0.0034942627 5.3215027e-4 0.0065517426 0.0025806427
;;  0.0054740906 0.0045757294 0.006351471 0.005428314 0.015485764 0.0075588226
;;  6.008148e-4 0.0015850067 9.3078613e-4 0.0037822723 0.005422592 0.0074825287
;;  0.0015068054 0.0025424957 0.015506744 6.1035156e-4 6.980896e-4 0.0025100708
;;  0.01540947 0.0018081665 0.00646019 0.0074443817 0.0015220642 0.00447464
;;  0.0024852753 0.006559372 0.0017566681 0.007347107 0.006587982 0.0025119781
;;  0.0026359558 0.0036373138 0.015331268 0.0016994476 0.0016727448 9.098053e-4
;;  0.0015125275 0.002571106 0.0037574768 0.002576828 0.0035362244 0.004512787
;;  0.0055732727 0.0064907074 0.0154953)




;; T => encoding + difference returns original float.
;; however, it does not always give the best encoding,
;; see *check-nearest-encoding*

;; Overflow maps to INFINITY but values that can be expressible in the
;; range of the float are encoded, notably NaN.

;; float (target)
;; ^
;; |                             .....
;; |
;; |                       === normalized
;; | ==========         === -INF
;; |                 === nan
;; |              === nan 
;; |           === nan
;; |
;; +-----------[#############################---> float (source)
;;
;;             ^ most negative float in the target encoding
;;         

(make-float-converters encode-float9* decode-float9* 4 4 t)

(with-open-file (out #P"/tmp/floats.data"
                     :direction :output
                     :if-exists :supersede)
  (flet ((enc (x) (encode-float9* x))
         (dec (x) (decode-float9* x)))
    (flet ((encode (f &aux
                        (e (multiple-value-list (enc f) ;; (encode f 2 3 t)
                                                ))
                        (bit (first e)))
             (multiple-value-call #'values
               bit
               (dec bit)
               (values-list (rest e)))))
      (loop
        for rat from -32 upto 32 by 1/50
        for float = (float rat)
        do (multiple-value-call
               #'format out "~&~@{~20A ~}~%"
             float
             (encode float))))))


