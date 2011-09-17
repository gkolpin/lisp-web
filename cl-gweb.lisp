(declaim (optimize (debug 3)))

(in-package :cl-gweb)

(defvar *debug* nil)
(defvar *dbg-errors* nil)
(defvar *acceptor* nil)
(defvar *cur-user-session* nil)
(defvar *user-sessions* (make-hash-table))
(defvar *user-sessions-lock* (make-lock))
(defvar *global-lock* (make-lock))
(defvar *callback-hash* nil)
(defvar *callback-required-hash* nil)
(defvar *init-fun* nil)
(defvar *port* 4343)
(defparameter base-url "/")
(defparameter input-url "/input")
(defvar *form-callback-hash* nil)
(defvar *radio-group-name*)
(defvar *radio-group-callback-map*)
(defvar *render-stream* nil)
(defvar *frame-key*)
(defvar *css-files* nil)
(defvar *js-files* nil)
(defvar *custom-dispatcher* nil)
(defvar *cur-widget* nil)

(defun start-gweb ()
  (when *dbg-errors*
    (setf *catch-errors-p* nil))
  (pushnew (get-action *custom-dispatcher* 'http-dispatch) *dispatch-table*)
  (hunchentoot:start (setf *acceptor* 
			   (make-instance 'acceptor
					  ;;:address "127.0.0.1"
					  :port *port*
					  :taskmaster (make-instance 
						       (if *debug*
							   'hunchentoot:single-threaded-taskmaster
							   'hunchentoot:one-thread-per-connection-taskmaster))))))

(defun stop-gweb ()
  (clrhash *user-sessions*)
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)))

(defun restart-gweb ()
  (stop-gweb)
  (start-gweb))

(def-fn-obj custom-dispatcher ((folder-dispatchers '()) (folder-uri-prefixes '()))
  (add-folder-dispatcher (uri-prefix base-path)
			 (unless (member uri-prefix folder-uri-prefixes :test 'equal)
			   (pushnew uri-prefix folder-uri-prefixes)
			   (pushnew (create-folder-dispatcher-and-handler uri-prefix base-path) folder-dispatchers)))
  (http-dispatch (request)
		 (dolist (dispatcher folder-dispatchers)
		   (awhen (funcall dispatcher request)
		     (return it)))))

(eval-when (:load-toplevel :execute)
  (unless *custom-dispatcher*
    (setf *custom-dispatcher* (create-custom-dispatcher))))

(defclass user-session ()
  ((hunchentoot-session :initarg :hunchentoot-session :accessor hunchentoot-session)
   (widget-tree :initarg :widget-tree :accessor widget-tree)
   (callbacks :initarg :callbacks :accessor callbacks)
   (frame-key :initform 1 :accessor frame-key)
   (callback-hash :initarg :callback-hash :accessor callback-hash)
   (announcer :initform (create-announcer) :accessor announcer)
   (widget-snapshots :initform (make-hash-table :test 'equal) :accessor widget-snapshots)
   (lock :initform (make-lock) :accessor lock)))

(defun initialize-user-session (hunchentoot-session)
  (let ((*cur-user-session* (make-instance 'user-session 
					   :hunchentoot-session hunchentoot-session
					   :callback-hash (make-hash-table :test 'equal))))
    (setf (widget-tree *cur-user-session*) (funcall *init-fun*))
    *cur-user-session*))

(defun store-user-session (user-session)
  (with-lock-held (*user-sessions-lock*)
    (setf (gethash (hunchentoot-session user-session) *user-sessions*) user-session)))

(defun retrieve-user-session (hunchentoot-session)
  (with-lock-held (*user-sessions-lock*)
    (gethash hunchentoot-session *user-sessions*)))

(define-easy-handler (input-handler :uri input-url)
    ((frame-key :real-name "k") (inputs :parameter-type 'hash-table) (submit-callbacks :parameter-type 'hash-table))
  (handle-gweb-request frame-key inputs submit-callbacks nil)
  (redirect 
   (with-lock-held ((lock (retrieve-user-session *session*)))
     (gen-new-frame-url base-url :frame-key frame-key))))

(define-easy-handler (root :uri base-url)
    ((frame-key :real-name "k") (inputs :parameter-type 'hash-table) (submit-callbacks :parameter-type 'hash-table))
  (handle-gweb-request frame-key inputs submit-callbacks t))

(defun handle-gweb-request (frame-key inputs submit-callbacks do-rendering)
  (setf (content-type*) "text/html")
  (no-cache)
  (with-lock-held (*global-lock*)
    (unless *session* (store-user-session (initialize-user-session (start-session))))
    (unless (retrieve-user-session *session*)
      (store-user-session (initialize-user-session (start-session)))))
  (with-lock-held ((lock (retrieve-user-session *session*)))
    (let* ((*cur-user-session* (retrieve-user-session *session*))
	   (*callback-hash* (callback-hash *cur-user-session*)))
      (unless frame-key (redirect (gen-new-frame-url base-url)))
      (let ((*frame-key* frame-key)
	    (*css-files* '())
	    (*js-files* '()))
	(evaluate-request frame-key inputs submit-callbacks :do-rendering do-rendering)))))

(defun remove-callbacks ()
  (clrhash *callback-hash*))

(defun evaluate-request (action-id inputs submit-callbacks &key (do-rendering t))
  ;; evaluate actions with optional inputs - perform before renders - perform renders
  (update-widgets-with-frame-key)
  (perform-action action-id inputs submit-callbacks)
  (remove-callbacks)
  (pre-render-widgets)
  (let ((*render-stream* (make-string-output-stream)))
    (when do-rendering (render-session-widgets))
    (store-widget-snapshots)
    (when do-rendering (get-output-stream-string *render-stream*))))
  
(defun perform-action (action-id &rest args)
  (awhen (gethash action-id *callback-hash*)
    (apply it args)))

(defun pre-render-widget-tree (widget-tree)
  (dolist (widget (widgets-in-tree widget-tree))
    (pre-render widget)))

(defun pre-render-widgets ()
  (pre-render-widget-tree (widget-tree *cur-user-session*)))

;; widgets should appear with parent widgets appearing before child widgets
(defun widgets-in-tree (widget-tree)
  (labels ((widgets-in-tree (widget-tree)
	     (if (not widget-tree)
		 '()
		 (cons widget-tree (mappend #'widgets-in-tree 
					    (child-widgets widget-tree))))))
    (widgets-in-tree widget-tree)))

(defmacro to-html (&body body)
  `(with-html-output (*render-stream*)
     ,@body))

(defun render-session-widgets ()
  (render-page (widget-tree *cur-user-session*)))

(defun render-page (widget)
  (to-html
    (:html 
     (:head
      (:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js" :type "text/javascript")
      (:script :src "http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js" :type "text/javascript")
      (dolist (css-file *css-files*)
	(htm (:link :rel "stylesheet"
		    :href css-file
		    :type "text/css"
		    :media "screen")))
      (:body
       (render widget)
       (dolist (js-file *js-files*)
	 (htm (:script :src js-file :type "text/javascript"))))))))

(defun store-widget-snapshots ()
  (dolist (widget (widgets-in-tree (widget-tree *cur-user-session*)))
    (take-snapshot widget)))

(defun update-widgets-with-frame-key ()
  (dolist (widget (widgets-in-tree (widget-tree *cur-user-session*)))
    (update-widget widget)))

(defgeneric render (component &optional view))

(defun add-folder-dispatcher (uri-prefix base-path)
  (send-message *custom-dispatcher* 'add-folder-dispatcher uri-prefix base-path))

(defun import-css (css-uri)
  (pushnew css-uri *css-files* :test 'equal))

(defun import-js (js-uri)
  (pushnew js-uri *js-files* :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;WIDGETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun slot-def-name (slot-def)
    (if (listp slot-def)
	(first slot-def)
	slot-def))

  (defun is-widget (slot-def)
    (if (listp slot-def)
	(find :widget slot-def)
	nil))
  
  (defun is-historied (slot-def)
    (if (listp slot-def)
	(find :historied slot-def)
	nil))
  
  (defun set-historied (slot-def)
    (if (atom slot-def)
	(list slot-def :historied)
	(append slot-def (list :historied)))))

(defgeneric child-widgets (widget))

(defun get-slot-history-value (obj slot-name)
  (if (aand (widget-snapshots *cur-user-session*)
	    (gethash *frame-key* it)
	    (gethash obj it)
	    (gethash slot-name it))
      (gethash slot-name (gethash obj (gethash *frame-key* (widget-snapshots *cur-user-session*))))
      (slot-value obj slot-name)))

(defun get-frame-history-hash ()
  (aif (gethash *frame-key* (widget-snapshots *cur-user-session*))
       it
       (let ((hash (make-hash-table)))
	 (setf (gethash *frame-key* (widget-snapshots *cur-user-session*)) hash)
	 hash)))

(defun get-obj-slot-hash (obj frame-hash)
  (aif (gethash obj frame-hash)
       it
       (let ((hash (make-hash-table)))
	 (setf (gethash obj frame-hash) hash)
	 hash)))

(defun set-slot-history-value (obj slot-name val)
  (setf (gethash slot-name (get-obj-slot-hash obj (get-frame-history-hash)))
	val))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun def-widget-readers (name slot-defs)
    (mapcar #'(lambda (slot-def)
		`(progn
		   (defgeneric ,(slot-def-name slot-def) (obj))
		   (defmethod ,(slot-def-name slot-def) ((obj ,name))
		     ,(if (is-historied slot-def)
			  `(get-slot-history-value obj ',(slot-def-name slot-def))
			  `(slot-value obj ',(slot-def-name slot-def))))))
	    slot-defs)))
  
(defmacro def-widget-with-history (name inherits-from slot-defs)
  `(defwidget ,name ,inherits-from ,(mapcar #'(lambda (slot-def)
						(set-historied slot-def))
					    slot-defs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun def-update-widget (class-name slot-defs)
    `(defmethod update-widget ((widget ,class-name))
       ,@(mapcar #'(lambda (slot-def)
		     (when (is-historied slot-def)
		       `(setf (,(slot-def-name slot-def) widget)
			      (get-slot-history-value widget 
						      ',(slot-def-name slot-def)))))
		 slot-defs)
       (call-next-method)))
  (defun def-widget-snapshotter (class-name slot-defs)
    `(defmethod take-snapshot ((widget ,class-name))
       ,@(mapcar #'(lambda (slot-def)
		     (when (is-historied slot-def)
		       `(set-slot-history-value widget
						',(slot-def-name slot-def)
						(,(slot-def-name slot-def) widget))))
		 slot-defs)
       (call-next-method))))

(defmacro defwidget (name inherits-from slot-defs)
  ;; slot-defs: (<name> :widget :initform <initform>)
  `(progn
     (defclass ,name ,inherits-from
       ,(mapcar #'(lambda (slot-args)
		    (destructuring-bind (slot-name &key (initform nil initform-p))
			slot-args
		      `(,slot-name
			:initarg ,(intern (symbol-name slot-name)
					  :keyword)
			:writer (setf ,slot-name)
			,@(when initform-p (list :initform initform)))))
		(mapcar #'(lambda (slot-def)
			    (if (atom slot-def)
				(list slot-def)
				(cons (slot-def-name slot-def)
				      (limit (member :initform slot-def) 2))))
			slot-defs)))
     (defun ,(cat-symbols 'create '- name) (&rest args)
       (apply #'make-instance (cons ',name args)))
     ,@(def-widget-readers name slot-defs)
     ,(def-widget-snapshotter name slot-defs)
     ,(def-update-widget name slot-defs)
     (defmethod child-widgets ((widget ,name))
       (list 
	,@(remove-nils
	   (mapcar #'(lambda (slot-def)
		       (when (is-widget slot-def)
			 `(,(slot-def-name slot-def) widget))) slot-defs))))))

(defwidget widget ()
  ((render-stack :initform '())
   (callback-stack :initform '())
   (rendering-for :initform nil)
   (id :initform nil)
   (style :initform "")))

;; (defclass widget ()
;;   ((render-stack :initform '() :accessor render-stack)
;;    (callback-stack :initform '() :accessor callback-stack)
;;    (rendering-for :initform nil :accessor rendering-for)))

(defmethod render ((widget widget) &optional (view t))
  (if (render-stack widget)
      (render-content (first (render-stack widget)) view)
      (render-content widget view)))

(defmethod render ((list list) &optional (view t))
  (render-content list view))

(defmethod render ((vector vector) &optional (view t))
  (render-content vector view))

;; takes a widget's value for the particular frame key and
;; sets its current slot value with the 'historical' value.
;; This is done before any actions are run, so that actions run
;; on the current slot value, and then the current slot values
;; are stored back to historical hashes.
(defgeneric update-widget (widget))

(defmethod update-widget ((widget t))
  (declare (ignore widget)))

(defgeneric take-snapshot (widget))

(defmethod take-snapshot ((widget t))
  (declare (ignore widget)))

(defgeneric before-render-content (widget &key))

(defgeneric render-content (widget view &key))

(defmethod render-content ((widget t) (view t) &key) "DEFAULT RENDERER")

(defmethod render-content :around ((widget widget) (view t) &key)
  (let ((*cur-widget* widget))
    (to-html
      (:div :id (id widget)
	    :class (ui-class widget view)
	    :style (style widget)
	    (call-next-method)))))

(defgeneric pre-render (component))

(defmethod pre-render ((widget t)))

(defmethod pre-render :around ((widget widget))
  (let ((*cur-widget* widget))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RENDERING UTILITIES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-unique-sym ()
  (intern (string-upcase (gen-callback-key))))

(defun gen-callback-key ()
  (concatenate 'string "k" 
	       (write-to-string (pincf (frame-key *cur-user-session*)))))

(defun gen-new-frame-url (url &key frame-key)
  (add-get-params-to-url url "k" (if frame-key frame-key (gen-callback-key))))

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

(defun prepare-callback (callback cur-widget)
  #'(lambda (&rest args)
      (let ((*cur-widget* cur-widget))
	(apply callback args))))

(defmacro with-link-callback ((callback-key-arg callback-fn) &body body)
  (with-gensyms (callback-fn-name)
    `(let ((,callback-key-arg (gen-callback-key))
	   (,callback-fn-name (prepare-callback ,callback-fn *cur-widget*)))
       (setf (gethash ,callback-key-arg *callback-hash*)
	     ,callback-fn-name)
       ,@body)))

(defmacro with-form-callback ((callback-key-arg callback-fn &optional (callback-required nil callback-required-p))
			      &body body)
  (with-gensyms (callback-fn-name)
    (let ((get-regular-callback
	   `(setf (gethash ,callback-key-arg *form-callback-hash*)
		  ,callback-fn-name)))
      `(let ((,callback-key-arg (gen-callback-key))
	     (,callback-fn-name (prepare-callback ,callback-fn *cur-widget*)))
	 ,(if callback-required-p
	      `(if ,callback-required
		   (setf (gethash ,callback-key-arg *callback-required-hash*)
			 ,callback-fn-name)
		   ,get-regular-callback)
	      get-regular-callback)
	 ,@body))))

(defun create-link (callback link-text)
  (let ((callback #'(lambda (inputs submit-inputs)
		      (declare (ignore inputs submit-inputs))
		      (funcall callback))))
    (with-link-callback (callback-key callback)
      (to-html
	(:a :href (gen-new-frame-url base-url :frame-key callback-key) (str link-text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html form utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro create-form (&optional form-opts &body body)
  (with-gensyms (frame-url-arg form-callback-key-arg form-callback-arg)
    `(let ((,form-callback-arg nil)
	   (,form-callback-key-arg (gen-callback-key)))
       (let* ((*form-callback-hash* (make-hash-table :test 'equal))
	      (*callback-required-hash* (make-hash-table :test 'equal))
	      (,frame-url-arg (gen-new-frame-url input-url :frame-key ,form-callback-key-arg)))
	 (to-html
	   (:form :id ,(getf form-opts :id) :method "POST" :action ,frame-url-arg
		  ,@body))
	 (let ((callback-hash *form-callback-hash*)
	       (callback-required-hash *callback-required-hash*))
	   (setf ,form-callback-arg
		 #'(lambda (inputs submit-callbacks)
		     (form-callback-fun callback-hash callback-required-hash
					inputs submit-callbacks)))))
       (setf (gethash ,form-callback-key-arg *callback-hash*)
	     ,form-callback-arg))))

(defmacro text-input (value &rest create-basic-input-args)
  `(create-basic-input ,value :text ,@create-basic-input-args))

(defmacro hidden-input (value &rest create-basic-input-args)
  `(create-basic-input ,value :hidden ,@create-basic-input-args))

(defun checkbox (&key checked callback on of on-true on-false)
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

(defun submit-input (value callback)
  (labels ((submit-callback (val)
	     (declare (ignore val))
	     (funcall callback)))
    (with-form-callback (callback-key #'submit-callback)
      (create-basic-input value :submit :name (format nil "~A{~A}" "submit-callbacks"
						      callback-key)
			  :callback #'submit-callback))))

(defun create-basic-input (value type &key id name callback on of checked
				       (callback-required nil) maxlength size)
  (let ((input-callback #'(lambda (val)
			     (cond (callback (funcall callback val))
				   ((and on of) (setf (slot-value of on) val))))))
    (with-form-callback (callback-key input-callback callback-required)
      (to-html
	(:input :type (symbol-name type)
		:id id
		:name (if name 
			  name
			  (format nil "~A{~A}" "inputs"
				  callback-key))
		:value value
		:checked (when checked "checked")
		:maxlength (when maxlength maxlength)
		:size (when size size))))))

(defun select-input (&key size values show selected callback on of)
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
	(to-html
	  (:select :size (when size (write-to-string size))
		   :name (format nil "~A{~A}" "inputs" callback-key)
		   (dolist (value value-input-map)
		     (htm (:option :value (write-to-string (first value))
				   :selected (when (eql (second value)
							selected)
					       "selected")
				   (esc (funcall show (second value))))))))))))

(defmacro with-radio-group (&body body)
  `(let ((*radio-group-name* (gen-callback-key))
	 (*radio-group-callback-map* (make-hash-table :test 'equal)))
     (to-html ,@body)
     (let ((radio-callback-map *radio-group-callback-map*)
	   (radio-group-name *radio-group-name*))
       (setf (gethash radio-group-name *form-callback-hash*)
	     #'(lambda (val)
		 (funcall (gethash val radio-callback-map)))))))

(defun radio-button (&key selected callback)
  (let ((callback-key (gen-callback-key))
	(callback (prepare-callback callback *cur-widget*)))
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

(defun date-input (&key callback with options)
  (let ((months '(january february march april may june july august september october
		  november december))
	(callback (prepare-callback callback *cur-widget*)))
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
	(let* ((now (today)))
	  (labels
	      ((month-html ()
		   (select-input :values months
				 :show #'(lambda (month) (format nil "~:(~a~)" month))
				 :callback month-select-callback
				 :selected (nth (1- (timestamp-month (if with with now))) months)))
	       (day-html ()
		 (text-input (timestamp-day (if with with now))
			     :callback day-callback :callback-required t
			     :size 2 :maxlength 2))
	       (year-html () (text-input (timestamp-year (if with with now))
					 :callback year-callback :callback-required t
					 :size 2 :maxlength 4)))
	    (to-html
	     (if options
		 (dolist (option options)
		   (case option
		     (:day (day-html))
		     (:year (year-html))
		     (:month (month-html))))
		 (progn
		   (month-html)
		   (day-html)
		   (year-html))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html control flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-widget (new-widget callback)
  (let ((callback (prepare-callback callback *cur-widget*))
	(cur-widget *cur-widget*))
    (if (rendering-for cur-widget)
	(progn
	  (push new-widget (render-stack (rendering-for cur-widget)))
	  (push callback (callback-stack (rendering-for cur-widget)))
	  (setf (rendering-for new-widget) (rendering-for cur-widget)))
	(progn
	  (push new-widget (render-stack cur-widget))
	  (push callback (callback-stack cur-widget))
	  (setf (rendering-for new-widget) cur-widget)))))

(defun answer (val)
  (let ((cur-widget *cur-widget*))
    (if (rendering-for cur-widget)
	(let ((callback (pop (callback-stack (rendering-for cur-widget)))))
	  (pop (render-stack (rendering-for cur-widget)))
	  (funcall callback val))
	(dolist (callback (callback-stack cur-widget))
	  (funcall callback val)))))

(defun on-answer (widget callback)
  (assert (not (rendering-for widget)))
  (let ((callback (prepare-callback callback *cur-widget*)))
    (push callback (callback-stack widget))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; announcers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass announcement () ())

(def-fn-obj announcer ((listeners (make-hash-table)))
  (register-listener (type action)
		     (setf (gethash type listeners) action))
  (announce (announcement)
	    (assert (typep announcement 'announcement))
	    (funcall (gethash (type-of announcement) listeners)
		     announcement)))

(defun register-listener (type action)
  (send-message (announcer *cur-user-session*) 'register-listener type action))

(defun announce (announcement)
  (assert (typep announcement 'announcement))
  (send-message (announcer *cur-user-session*) 'announce announcement))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass task (widget) ())

(defmethod child-widgets ((task task))
  (declare (ignore task))
  '())

(defmethod render ((task task) &optional (view t))
  (unless (render-stack task) (task-go task))
  (assert (render-stack task))
  (render-content (first (render-stack task)) view))

(defgeneric task-go (task))

(defmethod task-go :around ((task task))
  (let ((*cur-widget* task))
    (call-next-method)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lists as widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-content ((list list) (view t) &key)
  (to-html
   (dolist (widget list)
     (htm
      (render widget)))))

(defmethod child-widgets ((list list))
  list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays as widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-content ((vector vector) (view t) &key)
  (to-html
    (doarray (widget vector)
      (htm
       (render widget)))))

(defmethod child-widgets ((vector vector))
  (coerce vector 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings as widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod render-content ((string string) (view t) &key)
  (to-html (str string)))

(defmethod child-widgets ((string string)) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; allow widgets to set their own ui-class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ui-class (widget view))

(defmethod ui-class ((widget t) (view t))
  (string-downcase (symbol-name (type-of widget))))