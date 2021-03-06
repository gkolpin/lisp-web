(defpackage :cl-gweb
  (:use :common-lisp :hunchentoot :cl-who :url-rewrite :local-time :bordeaux-threads
	:lisp-utils)
  (:shadowing-import-from :hunchentoot #:url-encode)
  (:export #:*debug*
	   #:*dbg-errors*
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
	   #:checkbox
	   #:date-input
	   #:call-widget
	   #:answer
	   #:on-answer
	   #:announcement
	   #:register-listener
	   #:announce
	   #:task
	   #:task-go
	   #:*render-stream*
	   #:to-html
	   #:def-widget-with-history
	   #:create-fn-widget
	   #:add-folder-dispatcher
	   #:pre-render
	   #:import-css
	   #:import-js
	   #:style
	   #:gen-unique-sym
	   #:id
	   #:ui-class
	   #:render-page
	   #:pre-render-widget-tree
	   #:button-input
	   #:textarea-input))

(defpackage :test1
  (:use :common-lisp :cl-gweb :cl-who :local-time :lisp-utils))
