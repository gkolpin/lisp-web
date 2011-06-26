(in-package :test1)

(defwidget foo (widget) (a 
			 status
			 (b :widget)
			 (c :widget)
			 (call-test :widget)
			 (task-test :widget)))

(def-widget-with-history bar (widget) (a))

(defwidget test-form (widget) ((messages :initform '())))

(defclass status-announcement (announcement)
  ((status :initarg :status :accessor status)))

(defclass task-test (task) ())

(defun init-test1 ()
  (setf *init-fun* #'(lambda ()
		       (let* ((bar-widget (create-bar :a 3))
			      (foo-widget (create-foo :a 1 :b bar-widget
						      :status "no status"
						      :c (create-test-form)
						      :call-test (create-caller-widget)
						      :task-test (make-instance 'task-test))))
			 (on-answer bar-widget #'(lambda (val) (setf (a foo-widget) val)))
			 (register-listener 'status-announcement
					    #'(lambda (status)
						(setf (status foo-widget)
						      (status status))))
			 foo-widget)))
  (let ((*debug* t))
    (restart-gweb)))

(defmethod render-content ((widget foo) (view t) &key)
  (to-html
    (:html (:head)
	   (:body
	    (str (status widget))
	    (:br)
	    (str (a widget))
	    (:br)
	    (render (b widget))
	    (:br)
	    (render (c widget))
	    (:br)
	    (render (call-test widget))
	    (:br)
	    (render (task-test widget))))))

(defmethod render-content ((widget bar) (view t) &key)
  (to-html
    (str (a widget))
    (:br)
    (create-link #'(lambda () 
		     (incf (a widget))
		     (answer widget (a widget))
		     (announce (make-instance 'status-announcement
					      :status "incrementing!")))
		 "increment")
    (:br)
    (create-link #'(lambda ()
		     (announce (make-instance 'status-announcement
					      :status "no status")))
		 "reset status")))

(defmethod render-content ((widget test-form) (view t) &key)
  (let ((input-val nil))
    (create-form
      (dolist (message (messages widget))
	(htm
	 (str message)
	 (:br)))
      (hidden-input "input" :callback #'(lambda (val)
					  (push 
					   (concatenate 'string 
							"Hidden: "
							(write-to-string val))
					   (messages widget))))
      (select-input :size 2 :values '(a b c d) :show #'write-to-string 
		    :selected 'b
		    :callback #'(lambda (val)
				  (push
				   (concatenate 'string
						"select-val: "
						(write-to-string val))
				   (messages widget))))
      (:br)
      (with-radio-group
	(dotimes (n 2)
	  (htm
	   (let ((n n))
	     (radio-button :selected nil
			   :callback #'(lambda ()
					 (push
					  (concatenate 'string
						       "radio-button: "
						       (write-to-string n))
					  (messages widget))))
	   (esc (write-to-string n))))))
      (:br)
      (checkbox :checked t :callback #'(lambda (checked)
					 (push
					  (concatenate 'string
						       "checkbox: "
						       (if checked
							   "checked"
							   "NOT checked"))
					  (messages widget))))
      (:br)
      (date-input :callback #'(lambda (date)
				(push
				 (concatenate 'string
					      "date: "
					      (format-timestring nil date))
				 (messages widget)))
		  :with (encode-timestamp 0 0 0 0 27 1 1982)
		  :options '(:year :month :day))
      (:br)
      (create-link #'(lambda () (setf (messages widget) '())) "clear link")
      (:br)
      (submit-input "submit this"
		    #'(lambda ()
			;;(assert input-val)
			(setf input-val nil)
			(push
			 (concatenate 'string
				      "submit: "
				      (write-to-string "submit"))
			 (messages widget))))
      (submit-input "clear"
		    #'(lambda ()
			(setf (messages widget) '()))))))

(defwidget caller-widget (widget) ((answers :initform "answers: ")))

(defwidget answer-widget (widget) ())

(defwidget answer-widget2 (widget) ())

(defmethod render-content ((widget caller-widget) (view t) &key)
  (to-html
    (str (answers widget))
    (:br)
    (create-link #'(lambda () (call-widget (create-answer-widget)
					   widget
					   #'(lambda (answer)
					       (set-conc (answers widget)
							 answer))))
		 "Call answer widget")))

(defmethod render-content ((widget answer-widget) (view t) &key)
  (to-html
    (create-link #'(lambda () (call-widget (create-answer-widget2)
					   widget
					   #'(lambda (answer)
					       (answer widget
						       (concatenate 'string
								    "Answer "
								    answer)))))
		 "Answer")))

(defmethod render-content ((widget answer-widget2) (view t) &key)
  (to-html
    (create-link #'(lambda () (answer widget "answer2 "))
		 "Answer 2")))

(defmethod task-go ((task task-test))
  (labels ((task-loop ()
	     (call-widget (create-answer-widget)
			  task
			  #'(lambda (answer)
			      (declare (ignore answer))
			      (task-loop)))))
    (task-loop)))
