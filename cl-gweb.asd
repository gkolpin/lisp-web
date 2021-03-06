(defsystem cl-gweb
  :description "cl-gweb: A lisp web widgeting framework"
  :version "0.01"
  :author "Garrett Kolpin <gkolpin@gmail.com>"
  :depends-on (hunchentoot 
	       cl-who 
	       closer-mop
	       split-sequence
	       local-time
	       bordeaux-threads
	       lisp-utils)
  :components ((:file "cl-gweb" :depends-on ("packages"))
	       (:file "common-widgets" :depends-on ("packages" "cl-gweb"))
	       (:file "test-mocks" :depends-on("packages" "cl-gweb"))
	       (:file "packages")
	       (:file "test1" :depends-on ("packages" "cl-gweb"))))
