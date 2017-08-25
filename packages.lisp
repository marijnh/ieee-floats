(defpackage :ieee-floats
  (:use :common-lisp)
  (:import-from :alexandria
		#:with-gensyms
		#:ensure-list)
  (:export #:make-float-converters
	   #:encode-float32
	   #:decode-float32
	   #:encode-float64
	   #:decode-float64
	   #:with-float-converters))
