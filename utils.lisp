(in-package :ieee-floats)

(defun encode (float exponent-bits significand-bits special-values-p)
  (declare (type (or float (member
                            :not-a-number
                            :positive-infinity
                            :negative-infinity))
                 float)
           (optimize (debug 3)
                     (speed 0)
                     (space 0)
                     (safety 3)
                     (compilation-speed 3)))
  (let ((total-bits (+ 1 exponent-bits significand-bits))
        (exponent-offset  (1- (expt 2 (1- exponent-bits))))
        (significand-overflow (expt 2 significand-bits))
        (max-exponent (1- (expt 2 exponent-bits))))
    (flet ((encode (sign significand exponent remainder &aux (bits 0))
             ;; (declare (type (unsigned-byte total-bits) bits))
             (setf (ldb (byte 1 (1- total-bits)) bits) sign
                   (ldb (byte exponent-bits significand-bits) bits) exponent
                   (ldb (byte significand-bits 0) bits) significand)
             (values bits (typecase remainder
                            (symbol remainder)
                            (number (case sign
                                      (0 remainder)
                                      (1 (- remainder))))))))
      (if (and special-values-p (symbolp float))
          (ecase float
            (:not-a-number      (encode 0 1 max-exponent 0.0))
            (:positive-infinity (encode 0 0 max-exponent 0.0))
            (:negative-infinity (encode 1 0 max-exponent 0.0)))
          (multiple-value-bind (original-significand exponent sign) (decode-float float)
            (let ((sign (if (= sign 1.0) 0 1))
                  (biased-exponent (if (= 0 original-significand)
                                       exponent
                                       (+ exponent exponent-offset -1))))
              (multiple-value-bind (significand remainder)
                  (cond
                    ;; overflow
                    ((> biased-exponent max-exponent)
                     (values original-significand :overflow))

                    ;; (C)
                    ((plusp biased-exponent) 
                     ;; normalized
                     (multiple-value-bind (sig rem)
                         (round
                          (scale-float
                           (1- (* original-significand 2)) significand-bits))
                       (values sig (scale-float rem (- exponent
                                                       significand-bits
                                                       1)))))
                    (:denormalized
                     (multiple-value-bind (sig rem)
                         (round
                          (scale-float
                           original-significand
                           (+ significand-bits biased-exponent)))
                       (values sig (scale-float rem (- 1
                                                       exponent-offset
                                                       significand-bits))))))

                ;; Adjust so that we round up to the next exponent in
                ;; the target format.
                (when (= significand significand-overflow)
                  (incf biased-exponent)
                  (setf significand 0))

                (cond
                  ;; OVERFLOW
                  ((> biased-exponent max-exponent)
                   (encode sign 0 max-exponent :overflow))

                  ;; NORMALIZED
                  ((plusp biased-exponent)
                   (encode sign significand biased-exponent remainder))

                  ;; DENORMALIZED
                  ;; ((> biased-exponent (- significand-bits))
                  ;;  (encode sign significand  0 remainder))
                  
                  ;; UNDERFLOW (OR DENORMALIZED)
                  (t (encode sign significand 0 remainder))))))))))

(defun float-dice (exponent)
  (let ((offset (- (expt 2 (1- exponent))))
        (width (float (expt 2 exponent))))
    (lambda ()
      (+ (random width) offset))))

(defparameter *check-nearest-encoding* t)

(defun back-and-forth-p (float decoder exponent-bits significand-bits)
  (multiple-value-bind (bits difference)
      (encode float exponent-bits significand-bits t)
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

(defun make-back-and-forth-p (exponent-bits significand-bits)
  (with-float-converters (_ dec exponent-bits significand-bits nil)
    (lambda (float)
      (back-and-forth-p float #'dec exponent-bits significand-bits))))

(defun back-and-forth (times exp sig)
  (loop
    with dice = (float-dice exp)
    with test = (make-back-and-forth-p exp sig)
    repeat times
    for float = (funcall dice)
    for success = (funcall test float)
    unless success
      collect float))

(back-and-forth 1000000 4 4)

(proclaim '(optimize (debug 3)))

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

(with-open-file (out #P"/tmp/floats.data"
                     :direction :output
                     :if-exists :supersede)
  (with-float-converters (enc dec 2 3 :secondary)
    (flet ((encode (f &aux
                        (e (multiple-value-list (encode f 2 3 t)))
                        (bit (first e)))
             (multiple-value-call #'values
               bit
               (dec bit)
               (values-list (rest e)))))
      (loop
        for rat from -8 upto 8 by 1/50
        for float = (float rat)
        do (multiple-value-call
               #'format out "~&~@{~20A ~}~%"
             float
             (encode float))))))


