(in-package :test1)

(defwidget foo widget (a 
		       (b :widget)
		       (c :widget)
		       (call-test :widget)))

(defwidget bar widget (a))

(defwidget test-form widget ((messages :initform "")))

(defun init-test1 ()
  (setf *init-fun* #'(lambda ()
		       (let* ((bar-widget (create-bar :a 3))
			      (foo-widget (create-foo :a 1 :b bar-widget
						      :c (create-test-form)
						      :call-test (create-caller-widget))))
			 (on-answer bar-widget #'(lambda (val) (setf (a foo-widget) val)))
			 foo-widget)))
  (let ((*debug* t))
    (restart-gweb)))

(defmethod render-content ((widget foo) (view t) &key)
  (with-html-output-to-string (s)
    (:html (:head)
	   (:body
	    (str (a widget))
	    (:br)
	    (str (render (b widget)))
	    (:br)
	    (str (render (c widget)))
	    (:br)
	    (str (render (call-test widget)))))))

(defmethod render-content ((widget bar) (view t) &key)
  (with-html-output-to-string (s)
    (str (a widget))
    (:br)
    (str (create-link #'(lambda () 
			  (incf (a widget))
			  (answer widget (a widget)))
		      "increment"))))

(defmethod render-content ((widget test-form) (view t) &key)
  (let ((input-val nil))
    (create-form
      (str (messages widget))
      (hidden-input "input" :callback #'(lambda (val)
					  (set-conc (messages widget)
						    (html-to-string
						      "Hidden: "
						      (str (write-to-string val))
						      (:br)))))
      (select-input :size 2 :values '(a b c d) :show #'write-to-string 
		    :selected 'b
		    :callback #'(lambda (val)
				  (set-conc (messages widget)
					    (html-to-string
					      "select-val: "
					      (str (write-to-string val))
					      (:br)))))
      (:br)
      (with-radio-group
	(dotimes (n 2)
	  (htm
	   (let ((n n))
	     (radio-button :selected nil
			   :callback #'(lambda ()
					 (set-conc (messages widget)
						   (html-to-string
						     "radio-button: "
						     (str (write-to-string n))
						     (:br))))))
	   (esc (write-to-string n)))))
      (:br)
      (checkbox :checked t :callback #'(lambda (checked)
					 (set-conc (messages widget)
						   (html-to-string
						     "checkbox: "
						     (str (if checked
							      "checked"
							      "NOT checked"))
						     (:br)))))
      (:br)
      (date-input :callback #'(lambda (date)
				(set-conc (messages widget)
					  (html-to-string
					    "date: "
					    (str (format-timestring nil
								    date))
					    (:br))))
		  :with (encode-timestamp 0 0 0 0 27 1 1982)
		  :options '(:year :month :day))
      (:br)
      (create-link #'(lambda () (setf (messages widget) "")) "clear link")
      (:br)
      (submit-input "submit this"
		    #'(lambda ()
			;;(assert input-val)
			(setf input-val nil)
			(set-conc (messages widget)
				  (html-to-string
				    "submit: "
				    (str (write-to-string "submit"))
				    (:br) (:br)))))
      (submit-input "clear"
		    #'(lambda ()
			(setf (messages widget) ""))))))

(defwidget caller-widget widget ((answers :initform "answers: ")))

(defwidget answer-widget widget ())

(defwidget answer-widget2 widget ())

(defmethod render-content ((widget caller-widget) (view t) &key)
  (html-to-string
    (str (answers widget))
    (:br)
    (create-link #'(lambda () (call-widget (create-answer-widget)
					   widget
					   #'(lambda (answer)
					       (set-conc (answers widget)
							 answer))))
		 "Call answer widget")))

(defmethod render-content ((widget answer-widget) (view t) &key)
  (html-to-string
    (create-link #'(lambda () (call-widget (create-answer-widget2)
					   widget
					   #'(lambda (answer)
					       (answer widget
						       (concatenate 'string
								    "Answer "
								    answer)))))
		 "Answer")))

(defmethod render-content ((widget answer-widget2) (view t) &key)
  (html-to-string
    (create-link #'(lambda () (answer widget "answer2 "))
		 "Answer 2")))