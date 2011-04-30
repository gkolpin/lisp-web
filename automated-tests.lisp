(defpackage :automated-tests)
(in-package :automated-tests)
(use-package :cl-gweb)

(define-condition assertion-error (error) ())

(defun assertEql (a b)
  (unless (eql a b)
    (error 'assertion-error)))

(defun assert= (a b)
  (unless (= a b)
    (error 'assertion-error)))

(defun run-test (test-name)
  (format t "Running ~a... " (symbol-name test-name))
  (format t "~a"
	  (handler-case 
	      (progn
		(funcall test-name)
		"Passed")
	    (assertion-error () "Failed with assertion error")
	    (error () "Failed with error"))))
	  
(defun run-tests (&rest test-names)
  (dolist (test-name test-names)
    (run-test test-name)))

(defwidget foo widget (a 
		       (b :widget)))

(defwidget bar widget (a))

(defun test1 ()
  (setf *init-fun* #'(lambda () (create-foo :a 1 :b (create-bar :a 3))))
  (let ((widget-list (cl-gweb::widgets-in-tree (funcall *init-fun*))))
    (assert= (length widget-list) 2)
    (assertEql 'foo (type-of (first widget-list)))
    (assertEql 'bar (type-of (second widget-list)))))

(defun run-all-tests ()
  (run-tests
   'test1))
