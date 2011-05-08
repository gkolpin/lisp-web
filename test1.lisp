(in-package :test1)

(defwidget foo widget (a 
		       (b :widget)
		       (c :widget)))

(defwidget bar widget (a))

(defwidget test-form widget ())

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
  (create-form
    (create-submit-input "submit this" #'(lambda () (print "submitted")))))