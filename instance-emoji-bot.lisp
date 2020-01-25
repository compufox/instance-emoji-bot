;;;; instance-emoji-bot.lisp

(in-package #:instance-emoji-bot)

(defvar *emoji-list* nil)

(defvar *config-file* nil) 

(defvar *known-instances*
  (list "social.computerfox.xyz" "glaceon.social" "queer.party"
	"vulpine.club" "botsin.space" "cybre.space" "yiff.life"
	"donphan.social" "sleeping.town" "chomp.life"
	"computerfairi.es" "sanguine.space")
  "a default list of known instances")
(defvar *emoji-dir* (merge-pathnames "emojis/" (uiop:temporary-directory))
  "directory to save emojis")
(defvar *blacklist* nil
  "domain blacklist")


(defun parse-domain (text)
  (loop for word in (str:words text)
	for found = (cl-ppcre:scan-to-strings "(?:[^./,<>]+[.])*([^/.]+[.][^/.,<>]+)" word)
	when found
	  return found))

(defun parse-reply (notification)
  (when (mention-p notification)
    (let* ((status (tooter:status notification))
	   (domain (parse-domain (tooter:content status))))
      ;; scans the post for something that looks like a domain name
	(when (and domain
		   (not (member domain *blacklist* :test #'string=)))
	  (push domain *known-instances*)
	  (download-emoji-list domain)
	  (write-instance-list)
	  
	  (let ((emoji (choose-emoji domain)))
	    (loop for filename = (download-emoji emoji)      
		  until filename
			 
		  finally 
		     (reply status (format nil "~a from ~a" (agetf emoji :shortcode) domain)
			    :sensitive t
			    :media (download-emoji emoji)))))))))

(defun block-domain (status)
  ;; scans the post for something that looks like a domain name
  (let ((domain (parse-domain (tooter:content status))))
    (push domain *blacklist*)
    (setf *known-instances* (remove-if #'blocked-p *known-instances* :test #'string=))
    (write-instance-list)
    (write-blacklist)
    (reply status (format nil "blocked ~a" domain))))

(defun load-instance-list ()
  (setf *known-instances* (or (uiop:read-file-lines #P"instance.list"
						    :if-does-not-exist :create)
			      *known-instances*)))

(defun write-instance-list ()
  (str:to-file #P"instance.list" (str:join (string #\Newline) *known-instances*)))

(defun write-blacklist ()
  (str:to-file #P"blocked.list" (str:join (string #\Newline) *blacklist*)))

(defun load-blacklist ()
  (setf *blacklist* (uiop:read-file-lines #P"blocked.list")))  

(defun random-from-list (lst)
  (nth (random (length lst)) lst))

(defun load-emoji-file (instance)
  (str:from-file (concatenate 'string instance ".emojis")))

(defun choose-emoji (&optional domain)
  (let ((chosen-domain (or domain (random-from-list *known-instances*))))
    (values (random-from-list (decode-json-from-string (load-emoji-file chosen-domain)))
	    chosen-domain)))

(defun clean-downloads ()
  (mapcar #'delete-file (uiop:directory-files *emoji-dir*)))

(defun download-emoji (alist)
  (let ((filename (merge-pathnames (concatenate 'string
						(agetf alist :shortcode)
						"."
						(pathname-type (agetf alist :url)))
				   *emoji-dir*)))
    (handler-case
	(prog1 filename
	  (dex:fetch (agetf alist :url) filename) :if-exists nil)
      (error (e)
	nil))))

(defun update-emojis ()
  (mapcar #'download-emoji-list *known-instances*))

(defun download-emoji-list (instance)
  (handler-case
      (str:to-file (concatenate 'string instance ".emojis")
		   (dex:get (format nil "https://~a/api/v1/custom_emojis" instance)))
    (dex:http-request-not-found ()
      (push instance *blacklist*)
      (write-blacklist)
      nil)
    (error (e)
      nil)))

(defun main ()
  (handler-case
      (multiple-value-bind (opts args) (get-opts)
	(when (or (getf opts :help)
		  (every #'null opts args))
	  (unix-opts:describe
	   :prefix ""
	   :usage-of "instance-emoji-bot")
	  (uiop:quit 0))
	
	(if (getf opts :log)
	    (log:config :info)
	    (log:config :warn))
	
	(setf *config-file* (getf opts :config)))
    (unix-opts:missing-arg (e)
      (format t "ERROR: \"~a\" requires an argument~%" (unix-opts:option e))
      (format t "       view help for usage~%")
      (uiop:quit 1)))
  
  (load-instance-list)
  (ensure-directories-exist *emoji-dir*)
  (add-command "block" #'block-domain :privileged t)

  (handler-case
      (with-user-abort
	  (run-bot (make-instance 'mastodon-bot :config-file *config-file*
						:on-notification #'parse-reply)
	    (after-every (2 :hours :async t) (update-emojis))
	    (after-every (1 :day :async t) (clean-downloads))
	    (after-every (1 :hour)
	      (multiple-value-bind (emoji domain) (choose-emoji)
		(loop for filename = (download-emoji emoji)      
		      until filename
			 
		      finally 
			 (post (format nil "~a from ~a" (agetf emoji :shortcode) domain)
			       :cw "emoji"
			       :sensitive t
			       :media filename)))))
    (user-abort ()
      (format t "shutting down~%"))
    (error (e)
      (format t "hit error ~A~%" e)))))
  
