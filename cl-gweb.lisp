(declaim (optimize (debug 3)))

(in-package :cl-gweb)

(defvar *debug* nil)
(defvar *acceptor* nil)
(defvar *cur-user-session* nil)
(defvar *user-sessions* (make-hash-table))
(defvar *callback-hash* nil)
(defvar *callback-required-hash* nil)
(defvar *init-fun* nil)
(defvar *port* 4343)
(defparameter base-url "/cl-gweb")
(defvar *form-callback-hash* nil)
(defvar *radio-group-name*)
(defvar *radio-group-callback-map*)

(defun start-gweb ()
  (when *debug*
    (setf *catch-errors-p* nil))
  (hunchentoot:start (setf *acceptor* 
			   (make-instance 'acceptor
					  ;;:address "127.0.0.1"
					  :port *port*
					  :taskmaster (make-instance 
						       (if *debug*
							   'hunchentoot:single-threaded-taskmaster
							   'hunchentoot:one-thread-per-connection-taskmaster))))))

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

(defun remove-callbacks ()
  (clrhash *callback-hash*))

(defun evaluate-request (action-id inputs submit-callbacks)
  ;; evaluate actions with optional inputs - perform before renders - perform renders
  (perform-action action-id inputs submit-callbacks)
  (remove-callbacks)
  (pre-render-widgets)
  (render-session-widgets))
  
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

(defmacro setf-fn (place &optional val-modifier)
  (with-gensyms (val-arg)
    `#'(lambda (,val-arg)
	 (setf ,place 
	       ,(if val-modifier
		    `(funcall ,val-modifier ,val-arg)
		    val-arg)))))

(defmacro with-link-callback ((callback-key-arg callback-fn) &body body)
  `(let ((,callback-key-arg (gen-callback-key)))
     (setf (gethash ,callback-key-arg *callback-hash*)
	   ,callback-fn)
     ,@body))

(defmacro with-form-callback ((callback-key-arg callback-fn &optional (callback-required nil callback-required-p))
			      &body body)
  (let ((get-regular-callback
	  `(setf (gethash ,callback-key-arg *form-callback-hash*)
		 ,callback-fn)))
    `(let ((,callback-key-arg (gen-callback-key)))
       ,(if callback-required-p
	    `(if ,callback-required
		 (setf (gethash ,callback-key-arg *callback-required-hash*)
		       ,callback-fn)
		 ,get-regular-callback)
	    get-regular-callback)
       ,@body)))

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

(def-who-fun create-link (callback link-text)
  (let ((callback #'(lambda (inputs submit-inputs)
		      (declare (ignore inputs submit-inputs))
		      (funcall callback))))
    (with-link-callback (callback-key callback)
      (with-html-output-to-string (s)
	(:a :href (gen-new-frame-url :frame-key callback-key) (str link-text))))))

(defun form-callback-fun (callback-hash callback-required-hash
			  inputs submit-inputs)
  ;; perform regular input callbacks
  (maphash #'(lambda (input-key input-val)
	       (awhen (gethash input-key callback-hash) (funcall it input-val)))
  	   inputs)
  ;; perform required input callbacks
  (maphash #'(lambda (callback-key callback)
  	       (multiple-value-bind (val val-p) (gethash callback-key inputs)
  		 (if val-p (funcall callback val) (funcall callback nil))))
  	   callback-required-hash)
  ;; perform submit callbacks
  (maphash #'(lambda (submit-name submit-val)
   	       (funcall (gethash submit-name callback-hash) submit-val))
   	   submit-inputs))

(defmacro create-form (&body body)
  (with-gensyms (html-arg frame-url-arg form-callback-key-arg form-callback-arg)
    `(let ((,form-callback-arg nil)
	   (,form-callback-key-arg (gen-callback-key))
	   (,html-arg nil))
       (let* ((*form-callback-hash* (make-hash-table :test 'equal))
	      (*callback-required-hash* (make-hash-table :test 'equal))
	      (,frame-url-arg (gen-new-frame-url :frame-key ,form-callback-key-arg)))
	 (setf ,html-arg (html-to-string
			  (:form :method "POST" :action ,frame-url-arg
				 ,@body)))
	 (let ((callback-hash *form-callback-hash*)
	       (callback-required-hash *callback-required-hash*))
	   (setf ,form-callback-arg
		 #'(lambda (inputs submit-callbacks)
		     (form-callback-fun callback-hash callback-required-hash
					inputs submit-callbacks)))))
       (setf (gethash ,form-callback-key-arg *callback-hash*)
	     ,form-callback-arg)
       ,html-arg)))

(def-who-macro text-input (value &rest create-basic-input-args)
  `(create-basic-input ,value :text ,@create-basic-input-args))

(def-who-macro hidden-input (value &rest create-basic-input-args)
  `(create-basic-input ,value :hidden ,@create-basic-input-args))

(def-who-fun checkbox (&key checked callback on of on-true on-false)
  (let ((on-true-false-callback
	 #'(lambda (val)
	     (if val
		 (funcall on-true)
		 (funcall on-false)))))
    (if (and on-true on-false)
	(create-basic-input nil :checkbox :checked checked
			    :callback on-true-false-callback :callback-required t)
	(create-basic-input nil :checkbox	:checked checked 
			    :callback callback :on on :of of :callback-required t))))

(def-who-fun submit-input (value callback)
  (labels ((submit-callback (val)
	     (declare (ignore val))
	     (funcall callback)))
    (with-form-callback (callback-key #'submit-callback)
      (create-basic-input value :submit :name (format nil "~A{~A}" "submit-callbacks"
						      callback-key)
			  :callback #'submit-callback))))

(def-who-fun create-basic-input (value type &key name callback on of checked
				       (callback-required nil) maxlength size)
  (let ((input-callback #'(lambda (val)
			     (cond (callback (funcall callback val))
				   ((and on of) (setf (slot-value of on) val))))))
    (with-form-callback (callback-key input-callback callback-required)
      (html-to-string
	(:input :type (symbol-name type)
		:name (if name 
			  name
			  (format nil "~A{~A}" "inputs"
				  callback-key))
		:value value
		:checked (when checked "checked")
		:maxlength (when maxlength maxlength)
		:size (when size size))))))

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
      (with-form-callback (callback-key #'select-input-callback)
	(html-to-string
	  (:select :size (when size (write-to-string size))
		   :name (format nil "~A{~A}" "inputs" callback-key)
		   (dolist (value value-input-map)
		     (htm (:option :value (write-to-string (first value))
				   :selected (when (eql (second value)
							selected)
					       "selected")
				   (esc (funcall show (second value))))))))))))

(def-who-macro with-radio-group (&body body)
  (with-gensyms (html-arg)
    `(let ((*radio-group-name* (gen-callback-key))
	   (*radio-group-callback-map* (make-hash-table :test 'equal)))
       (let ((,html-arg (html-to-string ,@body)))
	 (let ((radio-callback-map *radio-group-callback-map*)
	       (radio-group-name *radio-group-name*))
	   (setf (gethash radio-group-name *form-callback-hash*)
		 #'(lambda (val)
		     (funcall (gethash val radio-callback-map)))))
	 ,html-arg))))

(def-who-fun radio-button (&key selected callback)
  (let ((callback-key (gen-callback-key)))
    (setf (gethash callback-key *radio-group-callback-map*) callback)
    (create-basic-input callback-key :radio
			:name (format nil "~A{~A}" "inputs" *radio-group-name*)
			:callback nil
			:checked selected)))

(defmacro with-prereq-callbacks (callback pre-requisite-callbacks &body body)
  (with-gensyms (prereqs-left-arg callback-arg)
    `(let ((,prereqs-left-arg ,(length pre-requisite-callbacks))
	   (,callback-arg ,callback))
       (let ,(mapcar #'(lambda (pre-req-callback)
			 `(,(first pre-req-callback)
			    #'(lambda (val)
				(decf ,prereqs-left-arg)
				(funcall ,(second pre-req-callback) val)
				(when (= 0 ,prereqs-left-arg)
				  (funcall ,callback-arg)))))
		     pre-requisite-callbacks)
	 ,@body))))

(def-who-fun date-input (&key callback with options)
  (let ((months '(january february march april may june july august september october
		  november december)))
    (bind-nil (month day year)
      (with-prereq-callbacks #'(lambda ()
				 (funcall callback
					  (encode-timestamp 0 0 0 0 day
							    (1+
							     (position month months))
							    year)))
	  ((month-select-callback (setf-fn month))
	   (day-callback (setf-fn day #'parse-integer))
	   (year-callback (setf-fn year #'parse-integer)))
	(let ((now (today)))
	  (html-to-string
	    (select-input :values months
			  :show #'(lambda (month) (format nil "~:(~a~)" month))
			  :callback month-select-callback
			  :selected (if (null with) (nth (1- (timestamp-month now))
							 months)))
	    (text-input (if (null with) (timestamp-day now))
			:callback day-callback :callback-required t
			:size 2 :maxlength 2)
	    (text-input (if (null with) (timestamp-year now))
			:callback year-callback :callback-required t
			:size 2 :maxlength 4)))))))
