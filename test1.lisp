(in-package :test1)

(defwidget foo widget (a 
		       (b :widget)))

(defwidget bar widget (a))

(defun init-test1 ()
  (setf *init-fun* #'(lambda () (create-foo :a 1 :b (create-bar :a "3"))))
  (let ((*debug* t))
    (restart-gweb)))

(defmethod render-content ((widget foo) (view t) &key)
  (render (b widget)))

(defmethod render-content ((widget bar) (view t) &key)
  (a widget))