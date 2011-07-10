(in-package :cl-gweb)

(defwidget fn-widget (widget)
  (render-fn))

(defmethod render-content ((widget fn-widget) (view t) &key)
  (funcall (render-fn widget)))
