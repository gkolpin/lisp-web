(defpackage :cl-gweb
  (:use :common-lisp :hunchentoot :cl-who :url-rewrite :local-time)
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
	   #:create-basic-input
	   #:hidden-input
	   #:text-input
	   #:submit-input
	   #:select-input
	   #:with-radio-group
	   #:radio-button
	   #:set-conc
	   #:html-to-string
	   #:checkbox
	   #:date-input
	   #:call-widget
	   #:answer
	   #:on-answer
	   #:announcement
	   #:register-listener
	   #:announce))

(defpackage :test1
  (:use :common-lisp :cl-gweb :cl-who :local-time))