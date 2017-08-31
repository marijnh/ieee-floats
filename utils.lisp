(defun encode (float)
  (declare
   (type
    (or float (member :not-a-number :positive-infinity :negative-infinity))
    float))
  (let (_ remain)
    (multiple-value-bind (sign significand exponent)
        (cond ((eq float :not-a-number) (values 0 1 7))
              ((eq float :positive-infinity) (values 0 0 7))
              ((eq float :negative-infinity) (values 1 0 7))
              (t
               (multiple-value-bind (significand exponent sign)
                   (decode-float float)
                 (let ((exponent
                         (if (= 0 significand)
                             exponent
                             (+ (1- exponent) 3)))
                       (sign
                         (if (= sign 1.0)
                             0
                             1)))
                   (unless (< exponent 8)
                     (restart-case
                         (error "floating point overflow when encoding ~a."
                                float)
                       (use-infinity ())))
                   (flet ((%ash (v)
                            (unless (or (zerop v)
                                        (> exponent -4))
                              (restart-case (error "Underflow")
                                (round-to-zero ())))
                            (ash v exponent)))
                     (if (<= exponent 0)
                         (values sign
                                 (%ash (multiple-value-setq (_ remain)
                                         (round (* 16 significand))))
                                 0)
                         (values sign (multiple-value-setq (_ remain)
                                        (round (* 16 (1- (* significand 2)))))
                                 exponent)))))))
      (let ((bits 0))
        (declare (type (unsigned-byte 8) bits))
        (setf (ldb (byte 1 7) bits) sign
              (ldb (byte 3 4) bits) exponent
              (ldb (byte 4 0) bits) significand)
        (values bits (/ remain (coerce 16 (type-of float))))))))

(defun encode (float exponent-bits significand-bits special-values-p)
  (declare (type (or float (member
                            :not-a-number
                            :positive-infinity
                            :negative-infinity))
                 float)
           (optimize (debug 3) (speed 1) (space 1) (safety 3)))
  (let ((total-bits (+ 1 exponent-bits significand-bits))
        (exponent-offset  (1- (expt 2 (1- exponent-bits))))
        (max-exponent (1- (expt 2 exponent-bits))))

    (multiple-value-bind (sign significand exponent remainder source)
        (cond
          ((and special-values-p (symbolp float))
           (ecase float
             (:not-a-number      (values 0 1 max-exponent))
             (:positive-infinity (values 0 0 max-exponent))
             (:negative-infinity (values 1 0 max-exponent))))
          (t
           (multiple-value-bind (significand exponent sign)
               (decode-float float)
             (let ((biased-exponent (if (= 0 significand)
                                        exponent
                                        (+ (1- exponent) exponent-offset)))
                   (sign (if (= sign 1.0) 0 1)))

               (cond
                 ;; Float is too large to be encoded (or would encode some NaN).
                 ;; Return positive or negative infinity. ADD TEST CASE.
                 ((>= biased-exponent max-exponent)
                  (values sign 0 max-exponent :overflow :overflow))

                 ;; (C)
                 ((plusp biased-exponent) ;; normalized
                  (block nil
                    (multiple-value-bind (new-significand remainder)
                        (round (* (expt 2 significand-bits)
                                  (1- (* significand 2))))
                      (when (= new-significand (expt 2 significand-bits))
                        (incf biased-exponent)
                        (setf new-significand 0)
                        (when (>= biased-exponent max-exponent)
                          (return-from nil
                            (values sign 0 max-exponent
                                    :overflow
                                    :round-overflow))))
                      
                      (values sign
                              new-significand
                              biased-exponent
                              (* (/ (float remainder float) (expt 2 significand-bits))
                                 (expt 2 (1- exponent)))
                              `(:positive-exponent :sig ,significand
                                                   :exp ,biased-exponent
                                                   :oex ,exponent
                                                   :rem ,remainder
                                                   :off ,exponent-offset
                                                   :new ,new-significand)))))

                 ;; (<= biased-exponent 0) ; subnormal
                 (t
                  
                  (block nil
                    (multiple-value-bind (new-significand remainder)
                        ;; float/round?
                        (round (scale-float significand (+ significand-bits biased-exponent)))
                      (when (= new-significand (expt 2 significand-bits))
                        (incf biased-exponent)
                        (setf new-significand 0)
                        (when (>= biased-exponent max-exponent)
                          (return-from nil
                            (values sign 0 max-exponent
                                    :overflow
                                    :round-overflow)))
                        (when (plusp biased-exponent)
                          (return
                            (values sign
                                    new-significand
                                    biased-exponent
                                    (scale-float remainder
                                              (- 1 (+ exponent-offset
                                                      significand-bits)))
                                    `(:positive-exponent :sig ,significand
                                                         :exp ,biased-exponent
                                                         :oex ,exponent
                                                         :rem ,remainder
                                                         :off ,exponent-offset
                                                         :new ,new-significand)))))
                      (cond
                        ((> biased-exponent (- significand-bits))
                         (values sign
                                 ;; (ash new-significand biased-exponent) ;; ???
                                 new-significand 
                                 0
                                 ;; one scale-float operation only ?
                                 (scale-float remainder
                                              (- 1 (+ exponent-offset
                                                      significand-bits)))
                                 `(:small-exponent :sign ,sign
                                                   :offset ,exponent-offset
                                                   :oexp ,exponent
                                                   :nsig ,(* 2 new-significand)
                                                   :ns ,new-significand
                                                   :os ,significand
                                                   :exp ,biased-exponent
                                                   :rem ,remainder)))
                        (t (values sign
                                   0
                                   0
                                   :underflow
                                   :underflow)))))))))))
      (let ((bits 0))
        ;;  (declare (type (unsigned-byte total-bits) bits))
        (setf (ldb (byte 1 (1- total-bits)) bits) sign
              (ldb (byte exponent-bits significand-bits) bits) exponent
              (ldb (byte significand-bits 0) bits) significand)
        (values bits remainder source)))))

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
          for failure = (multiple-value-bind (bits remain source)
                            (encode float . #1#)
                          (unless (symbolp remain)
                            (let* ((decoded (dec bits))
                                   (adjusted (+ remain decoded)))
                              (unless (= adjusted float)
                                (let* ((difference (- decoded float))
                                       (result (list :source source
                                                     :float float
                                                     :bits bits
                                                     :remain remain
                                                     :decoded decoded
                                                     :adjusted adjusted
                                                     :difference difference)))
                                  ;; (when (> difference 5)
                                  ;;   (warn "~S" result))
                                  result)))))
          when failure
            collect failure)))

