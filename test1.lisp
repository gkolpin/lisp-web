(in-package :test1)

(defwidget foo widget (a 
		       (b :widget)))

(defwidget bar widget (a))

(defun init-test1 ()
  (setf *init-fun* #'(lambda () (create-foo :a 1 :b (create-bar :a 3))))
  (let ((*debug* t))
    (start-gweb)))

(defmethod render ((widget foo) (view t) &key)
  (show-widget (b widget)))

(defmethod render ((widget bar) (view t) &key)
  (a widget))