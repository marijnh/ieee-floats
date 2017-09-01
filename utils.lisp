(defun encode (float exponent-bits significand-bits special-values-p)
  (declare (type (or float (member
                            :not-a-number
                            :positive-infinity
                            :negative-infinity))
                 float)
           (optimize (debug 3) (speed 1) (space 1) (safety 3)))
  (let ((total-bits (+ 1 exponent-bits significand-bits))
        (exponent-offset  (1- (expt 2 (1- exponent-bits))))
        (significand-overflow (expt 2 significand-bits))
        (max-exponent (1- (expt 2 exponent-bits))))
    (flet ((encode (sign significand exponent remainder &aux (bits 0))
             ;; (declare (type (unsigned-byte total-bits) bits))
             (setf (ldb (byte 1 (1- total-bits)) bits) sign
                   (ldb (byte exponent-bits significand-bits) bits) exponent
                   (ldb (byte significand-bits 0) bits) significand)
             (values bits remainder)))
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

              ;; First compute the significand and remainder, then adjust
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
                ;; the target format. The remainder is still the same,
                ;; that's why it was computed first.
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
                  ((> biased-exponent (- significand-bits))
                   (encode sign significand  0 remainder))
                  
                  ;; UNDERFLOW
                  (t (encode sign 0 0 float))))))))))))

(define-symbol-macro @
    (values
     (defparameter *cases*
       (coerce (test-back-and-forth 1000000 4 4) 'vector))
     (length *cases*)))

(defun @ (x) (elt *cases* x))

(defun test-back-and-forth (times exp sig)
  (with-float-converters (enc dec . #1=(exp sig nil))
    (loop repeat times
          for float = (* (expt 2 4) (random 0.1d0))
          for failure = (multiple-value-bind (bits remain)
                            (encode float . #1#)
                          (unless (symbolp remain)
                            (let* ((decoded (dec bits))
                                   (adjusted (+ remain decoded)))
                              (unless (= adjusted float)
                                (let* ((difference (- decoded float))
                                       (result (list :float float
                                                     :bits bits
                                                     :remain remain
                                                     :decoded decoded
                                                     :adjusted adjusted
                                                     :difference difference)))
                                  (when (> difference 5)
                                    (warn "~S" result))
                                  result)))))
          when failure
            collect failure)))



(with-open-file (out #P"/home/chris/tmp/floats.data"
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


