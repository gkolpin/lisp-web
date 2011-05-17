(in-package :cl-gweb)

(defvar *debug* nil)
(defvar *acceptor* nil)
(defvar *cur-user-session* nil)
(defvar *user-sessions* (make-hash-table))
(defvar *callback-hash* nil)
(defvar *init-fun* nil)
(defvar *port* 4343)
(defparameter base-url "/cl-gweb")

(defun start-gweb ()
  (when *debug*
    (setf *catch-errors-p* nil))
  (hunchentoot:start (setf *acceptor* 
			   (make-instance 'acceptor
					  ;;:address "127.0.0.1"
					  :port *port*))))
					  ;; :taskmaster (make-instance 
					  ;; 	       'hunchentoot:single-threaded-taskmaster)))))

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
   (frame-key :initform 1 :accessor frame-key)
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

(define-easy-handler (root :uri base-url)
    ((frame-key :real-name "k") (inputs :parameter-type 'hash-table) (submit-callbacks :parameter-type 'hash-table))
  (setf (content-type*) "text/html")
  (unless *session* (store-user-session (initialize-user-session (start-session))))
  (unless (retrieve-user-session *session*)
    (store-user-session (initialize-user-session (start-session))))
  (let* ((*cur-user-session* (retrieve-user-session *session*))
	 (*callback-hash* (callback-hash *cur-user-session*)))
    (unless frame-key (redirect (gen-new-frame-url)))
    (evaluate-request frame-key inputs submit-callbacks)))

(defun evaluate-request (action-id inputs submit-callbacks)
  ;; evaluate actions with optional inputs - perform before renders - perform renders
  (labels ((eval-callbacks (hashtable)
	     (maphash #'(lambda (input-key input-val)
			  (perform-action input-key input-val))
		      hashtable)))
    (eval-callbacks inputs)
    (eval-callbacks submit-callbacks)
    (perform-action action-id)
    (pre-render-widgets)
    (render-session-widgets)))
  
(defun perform-action (action-id &rest args)
  (awhen (gethash action-id *callback-hash*)
    (apply it args)))

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

(defgeneric child-widgets (widget))

(defmacro defwidget (name inherits-from slot-defs)
  ;; slot-defs: (<name> :widget :initform <initform>)
  `(progn
     (defclass ,name (,inherits-from)
       ,(mapcar #'(lambda (slot-args)
		    (destructuring-bind (slot-name &key (initform nil initform-p))
			slot-args
		      `(,slot-name
			:initarg ,(intern (symbol-name slot-name)
					  :keyword)
			:accessor ,slot-name
			,@(when initform-p (list :initform initform)))))
		(mapcar #'(lambda (slot-def)
			    (if (atom slot-def)
				(list slot-def)
				(cons (slot-def-name slot-def)
				      (limit (member :initform slot-def) 2))))
			slot-defs)))
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
	       (write-to-string (pincf (frame-key *cur-user-session*)))))

(defun gen-new-frame-url (&key frame-key)
  (add-get-params-to-url base-url "k" (if frame-key frame-key (gen-callback-key))))

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

(defmacro with-callback ((callback-key-arg callback-fn) &body body)
  `(let ((,callback-key-arg (gen-callback-key)))
     (setf (gethash ,callback-key-arg *callback-hash*)
	   ,callback-fn)
     ,@body))

(defun create-link (callback link-text)
  (with-callback (callback-key callback)
      (with-html-output-to-string (s)
	(:a :href (gen-new-frame-url :frame-key callback-key) (str link-text)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *who-fun-names* '())
  (defun register-who-fun (name)
    (pushnew name *who-fun-names*)))

(defmacro def-who-fun (name args &body body)
  (register-who-fun name)
  `(defun ,name ,args
     ,@body))

(defmacro def-who-macro (name lambda-list &rest body)
  (register-who-fun name)
  `(defmacro ,name ,lambda-list
     ,@body))

(defmacro html-to-string (&body body)
  (labels ((replace-who-functions (forms)
	     (mapcar #'(lambda (obj)
			 (cond ((and (consp obj) (find (first obj) *who-fun-names*))
				`(str ,obj))
			       ((consp obj) (replace-who-functions obj))
			       (t obj)))
		     forms)))
    (with-gensyms (string-arg)
      `(with-html-output-to-string (,string-arg)
	 ,@(replace-who-functions body)))))

(defmacro create-form (&body body)
  `(html-to-string
     (:form :method "POST" :action (gen-new-frame-url)
	    ,@body)))

(def-who-macro text-input (value &rest create-basic-input-args)
  `(create-basic-input ,value :text ,@create-basic-input-args))

(def-who-macro hidden-input (value &rest create-basic-input-args)
  `(create-basic-input ,value :hidden ,@create-basic-input-args))

(def-who-fun submit-input (value callback)
  (create-basic-input value :submit :callback callback))

(def-who-fun create-basic-input (value type &key callback on of)
  (let ((submit-callback #'(lambda (val)
			     (cond ((eql type :submit) (funcall callback))
				   (callback (funcall callback val))
				   ((and on of) (setf (slot-value of on) val))))))
    (with-callback (callback-key submit-callback)
      (html-to-string
	(:input :type (symbol-name type)
		:name (format nil "~A{~A}" (if (eql type :submit)
					       "submit-callbacks"
					       "inputs")
			      callback-key)
		:value value)))))

(def-who-fun select-input (&key size values show selected callback on of)
  (let ((value-input-map (let ((list-idx 0))
			   (mapcar #'(lambda (value)
				       (list (pincf list-idx)
					     value))
				   values))))
    (labels ((select-input-callback (value)
	       (let ((int-value (parse-integer value)))
		 (if callback
		     (funcall callback (second (assoc int-value value-input-map)))
		     (setf (slot-value of on) value)))))
      (with-callback (callback-key #'select-input-callback)
	(html-to-string
	  (:select :size (when size (write-to-string size))
		   :name (format nil "~A{~A}" "inputs" callback-key)
		   (dolist (value value-input-map)
		     (htm (:option :value (write-to-string (first value))
				   :selected (when (eql (second value)
							selected)
					       "selected")
				   (esc (funcall show (second value))))))))))))