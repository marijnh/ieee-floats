(defsystem :ieee-floats
  :components ((:file "ieee-floats")))

(defsystem :ieee-floats-tests
  :description "Convert floating point values to IEEE 754 binary representation"
  :author "Marijn Haverbeke <marijnh@gmail.com>"
  :license "BSD"
  :depends-on (:ieee-floats :eos)
  :components ((:file "tests")))
