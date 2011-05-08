(defpackage :automated-tests (:use :common-lisp :cl-gweb))
(in-package :automated-tests)
(use-package :cl-gweb)

(define-condition assertion-error (error) ())

(defun assertEqual (a b)
  (unless (equal a b)
    (error 'assertion-error)))

(defun assertEql (a b)
  (unless (eql a b)
    (error 'assertion-error)))

(defun assert= (a b)
  (unless (= a b)
    (error 'assertion-error)))

(defun run-test (test-name)
  (format t "Running ~a... " (symbol-name test-name))
  (format t "~a~%"
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

(defmethod render-content ((widget foo) (view t) &key)
  (render (b widget)))

(defmethod render-content ((widget bar) (view t) &key)
  (a widget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun setup-user-session1 ()
  (setf *init-fun* #'(lambda () (create-foo :a 1 :b (create-bar :a "3"))))
  (cl-gweb::initialize-user-session nil))

(defun test1 ()
  (let* ((cl-gweb::*cur-user-session* (setup-user-session1))
	 (widget-list (cl-gweb::widgets-in-tree
		       (cl-gweb::widget-tree cl-gweb::*cur-user-session*))))
    (assert= (length widget-list) 2)
    (assertEql 'foo (type-of (first widget-list)))
    (assertEql 'bar (type-of (second widget-list)))))

(defun test-render ()
  (let* ((cl-gweb::*cur-user-session* (setup-user-session1))
	 (cl-gweb::*callback-hash* (make-hash-table)))
    ;;(cl-gweb::evaluate-request nil nil)
    (assertEqual "3" (cl-gweb::render
		      (cl-gweb::widget-tree cl-gweb::*cur-user-session*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-all-tests ()
  (run-tests
   'test1
   'test-render))
