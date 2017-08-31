(in-package :ieee-floats)

(define-condition conversion-error (error) ())

(define-condition underflow (conversion-error) ())
(define-condition overflow (conversion-error) ())

(deftype bit-length () `(integer 0))
(deftype nan-and-infinity-option ()
  `(member t nil :secondary))
