(in-package :cl-gweb)

(defvar *debug* nil)
(defvar *acceptor* nil)
(defvar *cur-user-session* nil)
(defvar *user-sessions* (make-hash-table))
(defvar *callback-hash* nil)
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
   (widget-tree :initarg :widget-tree :accessor widget-tree)
   (callbacks :initarg :callbacks :accessor callbacks)
   (callback-key :initform 1 :accessor callback-key)
   (callback-hash :initarg :callback-hash :accessor callback-hash)))

(defun initialize-user-session (hunchentoot-session)
  (let ((widget-tree (funcall *init-fun*)))
    (make-instance 'user-session 
		   :hunchentoot-session hunchentoot-session
		   :widget-tree widget-tree
		   :callback-hash (make-hash-table :test 'equal))))

(defun store-user-session (user-session)
  (setf (gethash (hunchentoot-session user-session) *user-sessions*) user-session))

(defun retrieve-user-session (hunchentoot-session)
  (gethash hunchentoot-session *user-sessions*))

(defun gather-inputs ()
  '())

(define-easy-handler (root :uri "/cl-gweb")
    ((interaction-id :parameter-type 'integer)
     action-id)
  (setf (content-type*) "text/html")
  (unless interaction-id (redirect (add-get-param-to-url "/cl-gweb" 
							 "interaction-id" "1")))
  (unless *session* (store-user-session (initialize-user-session (start-session))))
  (let* ((*cur-user-session* (retrieve-user-session *session*))
	 (inputs (gather-inputs))
	 (*callback-hash* (callback-hash *cur-user-session*)))
    (evaluate-request action-id inputs)))

(defun evaluate-request (action-id inputs)
  ;; evaluate actions with optional inputs - perform before renders - perform renders
  (perform-action action-id)
  (pre-render-widgets)
  (render-session-widgets))

(defun perform-action (action-id)
  (awhen (gethash action-id *callback-hash*)
    (funcall it)))

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
  (render (widget-tree *cur-user-session*)))

(defun render (widget)
  (render-content widget t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;WIDGETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass widget ()
  ())

(defgeneric before-render-content (widget &key))

(defgeneric render-content (widget view &key))

(defmethod render-content ((widget t) (view t) &key) "DEFAULT RENDERER")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RENDERING UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-callback-key ()
  (concatenate 'string "k" 
	       (write-to-string (pincf (callback-key *cur-user-session*)))))

(defun add-get-params-to-url (url &rest name-value-pairs)
  (labels ((rec (name-value-pairs url)
	     (if (not name-value-pairs)
		 url
		 (rec (cddr name-value-pairs)
		      (add-get-param-to-url url
					    (first name-value-pairs)
					    (second name-value-pairs))))))
    (rec name-value-pairs url)))

(defmacro link-fn (fn &rest args)
  `#'(lambda ()
       (,fn ,@args)))

(defun create-link (callback link-text)
  (let ((callback-key (gen-callback-key)))
    (setf (gethash callback-key *callback-hash*)
	  callback)
    (with-html-output-to-string (s)
      (:a :href (add-get-params-to-url "/cl-gweb" 
				       "interaction-id" "1"
				       "action-id" callback-key) (str link-text)))))