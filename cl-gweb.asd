(defsystem cl-gweb
  :description "cl-gweb: A lisp web widgeting framework"
  :version "0.01"
  :author "Garrett Kolpin <gkolpin@gmail.com>"
  :depends-on (hunchentoot cl-who closer-mop split-sequence)
  :components ((:file "cl-gweb" :depends-on ("packages" "utils"))
	       (:file "utils" :depends-on ("packages"))
	       (:file "packages")
	       (:file "test1" :depends-on ("packages" "cl-gweb"))))
