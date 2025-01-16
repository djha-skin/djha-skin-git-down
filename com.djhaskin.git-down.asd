(defsystem "com.djhaskin.gag"
  :version "0.1.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "quri"
               "serapeum"
               "com.djhaskin.cliff"
               "trivial-features"
               "trivial-package-local-nicknames"
               )
  :components ((:module "src"
                :components
                ((:file "errors")
                 (:file "main"))))
  :description "CLI library for Common Lisp. Handles args, env vars, and conf"
  :in-order-to ((test-op (test-op "com.djhaskin.gag/tests"))))

(defsystem "com.djhaskin.gag/tests"
  :version "0.10.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
      "com.djhaskin.gag"
               "parachute")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for gag"
  :perform (asdf:test-op (op c)

                         (uiop:symbol-call
                           :parachute
                           :test :com.djhaskin.nrdl/tests)))
