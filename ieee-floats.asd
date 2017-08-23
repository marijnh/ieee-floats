(defsystem :ieee-floats
  :components ((:file "ieee-floats")))
  :in-order-to ((test-op (test-op "ieee-floats-tests"))))

(defsystem :ieee-floats-tests
  :description "Convert floating point values to IEEE 754 binary representation"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "BSD"
  :depends-on (:ieee-floats :eos)
  :components ((:file "tests"))
  :perform (test-op (o s) (uiop:symbol-call :eos '#:run! :ieee-floats)))
