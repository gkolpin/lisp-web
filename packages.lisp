(defpackage :cl-gweb
  (:use :common-lisp :hunchentoot :cl-who)
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
	   #:show-widget))

(defpackage :test1
  (:use :common-lisp :cl-gweb))