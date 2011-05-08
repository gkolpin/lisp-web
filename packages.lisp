(defpackage :cl-gweb
  (:use :common-lisp :hunchentoot :cl-who :url-rewrite)
  (:shadowing-import-from :hunchentoot #:url-encode)
  (:export #:*debug*
	   #:render
	   #:render-content
	   #:before-render
	   #:*init-fun*
	   #:initialize-widgets
	   #:widget
	   #:start-gweb
	   #:restart-gweb
	   #:defwidget
	   #:show-widget
	   #:link-fn
	   #:create-link
	   #:create-form
	   #:create-text-input
	   #:create-submit-input))

(defpackage :test1
  (:use :common-lisp :cl-gweb :cl-who))