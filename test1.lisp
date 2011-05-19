(in-package :test1)

(defwidget foo widget (a 
		       (b :widget)
		       (c :widget)))

(defwidget bar widget (a))

(defwidget test-form widget ((messages :initform "")))

(defun init-test1 ()
  (setf *init-fun* #'(lambda () (create-foo :a 1 :b (create-bar :a 3)
					    :c (create-test-form))))
  (let ((*debug* t))
    (restart-gweb)))

(defmethod render-content ((widget foo) (view t) &key)
  (with-html-output-to-string (s)
    (:html (:head)
	   (:body
	    (str (render (b widget)))
	    (:br)
	    (str (render (c widget)))))))

(defmethod render-content ((widget bar) (view t) &key)
  (with-html-output-to-string (s)
    (str (a widget))
    (:br)
    (str (create-link (link-fn incf (a widget))
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