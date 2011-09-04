(in-package :cl-gweb)

;; A widget will have a reference to the last 'real' widget that was rendered
;; in the rendering stack
(defwidget fn-widget (widget)
  (render-fn last-widget))

(defmethod render-content ((widget fn-widget) (view t) &key)
  (let ((*cur-widget* (last-widget widget))) (funcall (render-fn widget))))

(defun call-fn (fn cont)
  (call-widget (create-fn-widget :render-fn fn :last-widget *cur-widget*) cont))