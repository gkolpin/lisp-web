(in-package :cl-gweb)

(defvar *debug* nil)
(defvar *acceptor* nil)
(defvar *cur-user-session* nil)
(defvar *user-sessions* (make-hash-table))
(defvar *widget-hash* nil)
(defvar *init-fun* nil)
(defvar *port* 4343)

(defun start-gweb ()
  (when *debug*
    (setf *catch-errors-p* nil))
  (hunchentoot:start (setf *acceptor* 
			   (make-instance 'acceptor
					  ;;:address "127.0.0.1"
					  :port *port*))))

(defun stop-gweb ()
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))

(defun restart-gweb ()
  (stop-gweb)
  (start-gweb))

(defclass user-session ()
  ((hunchentoot-session :initarg :hunchentoot-session :accessor hunchentoot-session)
   (widget-tree :initarg :widget-tree :accessor widget-tree)))

(defun initialize-user-session (hunchentoot-session)
  (let ((widget-tree (funcall *init-fun*)))
    (make-instance 'user-session 
		   :hunchentoot-session hunchentoot-session
		   :widget-tree widget-tree)))

(defun store-user-session (user-session)
  (setf (gethash (hunchentoot-session user-session) *user-sessions*) user-session))

(defun retrieve-user-session (hunchentoot-session)
  (gethash hunchentoot-session *user-sessions*))

(defun gather-inputs ()
  '())

(define-easy-handler (root :uri "/") ((interaction-id :parameter-type 'integer))
  (setf (content-type*) "text/html")
  (unless *session* (store-user-session (initialize-user-session (start-session))))
  (let ((*cur-user-session* (retrieve-user-session *session*))
	(inputs (gather-inputs)))
    (let ((*widget-hash* (make-hash-table)))
      (evaluate-request inputs)
      (with-html-output-to-string (s)
	(str (show-widget-tree (widget-tree *cur-user-session*)))))))

(defun evaluate-request (inputs)
  ;; evaluate actions with optional inputs - perform before renders - perform renders
  (perform-actions)
  (pre-render-widgets)
  (render-session-widgets))

(defun perform-actions ()
  )

(defun pre-render-widgets ()
  )

;; widgets should appear with parent widgets appearing before child widgets
(defun widgets-in-tree (widget-tree)
  (labels ((widgets-in-tree (widget-tree)
	     (if (not widget-tree)
		 '()
		 (cons widget-tree (mappend #'widgets-in-tree 
					    (child-widgets widget-tree))))))
    (widgets-in-tree widget-tree)))
		 
(defun render-session-widgets ()
    (dolist (widget (reverse (widgets-in-tree (widget-tree *cur-user-session*))))
      (setf (gethash widget *widget-hash*) (render widget t))))

;; called by render methods - assumes widget has already been rendered
;; and exists in *widget-hash*
(defun show-widget (widget)
  (gethash widget *widget-hash*))
  
(defun show-widget-tree (widget-tree)
  (show-widget widget-tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;WIDGETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass widget ()
  ())

(defgeneric before-render (widget &key))

(defgeneric render (widget view &key))

(defmethod render ((widget t) (view t) &key) "DEFAULT RENDERER")

(defun slot-def-name (slot-def)
  (if (listp slot-def)
      (first slot-def)
      slot-def))

(defun is-widget (slot-def)
  (if (listp slot-def)
      (find :widget slot-def)
      nil))

(defmacro defwidget (name inherits-from slot-defs)
  ;; slot-defs: (<name> :widget)
  `(progn
     (defclass ,name (,inherits-from)
       ,(mapcar #'(lambda (slot-name) `(,slot-name
					:initarg ,(intern (symbol-name slot-name)
							  :keyword)
					:accessor ,slot-name)) 
		(mapcar #'slot-def-name slot-defs)))
     (defun ,(cat-symbols 'create '- name) (&key ,@(mapcar #'slot-def-name slot-defs))
       (make-instance ',name ,@(apply #'append
				      (mapcar #'(lambda (slot-name)
						  `(,(to-keyword slot-name) ,slot-name))
					      (mapcar #'slot-def-name slot-defs)))))
     (defmethod child-widgets ((widget ,name))
       (list 
	,@(remove-nils
	   (mapcar #'(lambda (slot-def)
		       (when (is-widget slot-def)
			 `(,(slot-def-name slot-def) widget))) slot-defs))))))
