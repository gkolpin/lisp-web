(asdf:operate 'asdf:load-op :cl-gweb)
(load "automated-tests.lisp")
(automated-tests::run-all-tests)
(quit)