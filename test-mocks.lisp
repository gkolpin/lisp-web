(in-package :cl-gweb)

(defmacro with-mock-session (&body body)
  `(let* ((*init-fun* #'(lambda ()))
	  (*cur-user-session* (initialize-user-session nil))
	  (*callback-hash* (make-hash-table :test 'equal)))
     ,@body))
  