(defsystem #:com.djhaskin.qp
  :version "0.4.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "com.djhaskin.nrdl"
               "com.djhaskin.cliff"
               "alexandria"
               "trivial-features"
               "cl-ppcre"
               "arrows"
               )
  :components ((:module "src"
          :components
          ((:file "main"))))
  :description "Nestable Readable Document Language"
  :in-order-to (
                (test-op (test-op "com.djhaskin.qp/tests"))))

(defsystem #:com.djhaskin.qp/tests
  :version "0.4.0"
  :author "Daniel Jay Haskin"
  :license "MIT"
  :depends-on (
               "com.djhaskin.qp"
               "parachute")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for RSS syncer"
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call
                           :parachute
                           :test :com.djhaskin.qp/tests)))
