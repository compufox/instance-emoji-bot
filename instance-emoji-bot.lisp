;;;; instance-emoji-bot.lisp

(in-package #:instance-emoji-bot)

(defvar *emoji-list* nil)

(defvar *config-file* nil) 

(defvar *known-instances*
  (list "social.computerfox.xyz" "glaceon.social" "queer.party"
	"botsin.space" "cybre.space" "yiff.life"
	"donphan.social" "sleeping.town" "chomp.life"
	"computerfairi.es" "sanguine.space" "monsterpit.net"
	"glaceon.social")
  "a default list of known instances")

(defvar *emoji-dir* (merge-pathnames "emojis/" (uiop:temporary-directory))
  "directory to save emojis")

(defvar *blacklist*
  (list "gab.com" "spinster.xyz" "glindr.org" "gleasonator.com"
	"imvegan.fyi" "freespeachextremist.com" "develop.gab.com"
	"gab.sleeck.eu" "liberdon.com" "neckbeard.xyz"
	"qoto.org" "pleroma.kiwifarms.net" "gab.ai" "gabfed.com"
	"gab.io" "gab.polaris-1.work" "gab.protohype.net"
	"kiwifarms.is" "kiwifarms.cc" "ms-olive.club"
	"spinster.dev" "yang.social")
  "domain blacklist")

(defvar *no-domain-message* "sorry, i cant parse a domain name, please try again")
(defvar *restricted-message* "sorry, i cant fetch from there")
(defvar *download-error* "sorry, i was unable to get emojis for there :c")

(defun parse-domain (text)
  (loop for word in (str:words text)
	for found = (cl-ppcre:scan-to-strings "(?:[^./,<>]+[.])*([^/.]+[.][^/.,<>]+)" word)
	when found
	  return found))

(defun blocked-p (domain)
  (member domain *blacklist* :test #'equal))

(defun parse-reply (notification)
  (when (mention-p notification)
    (let* ((status (tooter:status notification))
	   (domain (parse-domain (tooter:content status))))
      ;; scans the post for something that looks like a domain name
      (if domain
	  (unless (blocked-p domain)
	    (download-emoji-list domain)
	    
	    (let ((emoji (choose-emoji :domain domain
				       :retry-count 10)))
	      (if emoji
		  (loop for filename = (download-emoji emoji)      
			until filename
			
			finally
			   (when (log:info)
			     (log:info "replying to" (tooter:id status)
				       "with" (agetf emoji :shortcode) "from" domain))

			   ;; if we're here then everything went okay!
			   (reply status (format nil "~a from ~a"
						 (agetf emoji :shortcode)
						 domain)
				  :sensitive t
				  :media (download-emoji emoji))
			   
			   ;; only write the instance list here, because it means
			   ;;  we were able to correctly ping the site
			   (push domain *known-instances*)
			   (setf *known-instances*
				 (remove-duplicates *known-instances* :test #'string=))
			   (write-instance-list))

		  ;; if we're here then we failed to choose an emoji, which means that
		  ;;  we failed to download the list
		  (reply status *download-error*))))

	  ;; if we're here then we failed to parse a domain name from
	  ;;  the mention we received
	  (reply status *no-domain-message*)))))

(defun block-domain (status)
  ;; scans the post for something that looks like a domain name
  (let ((domain (parse-domain (tooter:content status))))
    (push domain *blacklist*)
    (setf *known-instances*
	  (remove-if #'blocked-p *known-instances* :test #'string=))
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
  (uiop:read-file-string (concatenate 'string instance ".emojis")))

(defun choose-emoji (&key domain retry-count)
  (let ((chosen-domain (or domain (random-from-list *known-instances*))))
    (if (emoji-file-exists-p chosen-domain)
	(values (random-from-list
		 (decode-json-from-string (load-emoji-file chosen-domain)))
		chosen-domain)
	(if (>= retry-count 1)
	    (progn
	      (download-emoji-list domain)
	      (choose-emoji :domain domain
			    :retry-count (1- retry-count)))))))

(defun emoji-file-exists-p (domain)
  (uiop:file-exists-p (concatenate 'string domain ".emojis")))

(defun clean-downloads ()
  (mapcar #'delete-file (uiop:directory-files *emoji-dir*)))

(defun download-emoji (alist)
  (let* ((extension (pathname-type (first (str:split #\? (agetf alist :url)))))
	 (filename (merge-pathnames (concatenate 'string
						 (agetf alist :shortcode)
						 "."
						 extension)
				   *emoji-dir*)))
    (handler-case
	(prog1 filename
	  (dex:fetch (agetf alist :url) filename :if-exists nil))
      (error (e)
	(log:warn "encountered error:" e
		  "trying to fetch" (agetf alist :url))
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
      (log:warn "404'd on" instance)
      nil)
    (dex:http-request-unauthorized ()
      (log:warn "oof," instance "probably has auth'd fetch enabled :c")
      nil)
    (error (e)
      (log:warn "hit error when fetching emoji for" instance)
      nil)))

(defun main ()
  (handler-case
      (multiple-value-bind (opts args) (get-opts)
	(when (or (getf opts :help)
		  (every #'null opts))
	  (unix-opts:describe
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
	  (run-bot ((make-instance 'mastodon-bot :config-file *config-file*
				   :on-notification #'parse-reply)
		    :delete-command t)
		   
	    ;; update the emoji lists every 2 days
	    (after-every (2 :days :async t :run-immediately t)
	      (when (log:info)
		(log:info "updating emojis"))
	      (update-emojis))

	    ;; clean out our saved emojis every week
	    (after-every (1 :week :async t)
	      (when (log:info)
		(log:info "cleaning emojis"))
	      (clean-downloads))

	    ;; every hour post an emoji
	    (after-every (1 :hour)
	      (tagbody choose
	        (multiple-value-bind (emoji domain) (choose-emoji :retry-count 10)
		  (loop with count = 10
			for filename = (download-emoji emoji)
		        until filename

			do (setf count (1- count))
			
			if (zerop count)
			do (go choose)
			
		        finally
		       	 (when (log:info)
			   (log:info "posting emoji" (agetf emoji :shortcode)
				     "from" domain))
			 (post (format nil "~a from ~a"
				       (agetf emoji :shortcode)
				       domain)
			       :cw "emoji"
			       :sensitive t
			       :media filename)))))))
    (user-abort ()
      (when (log:info)
	(log:info "shutting down")))
    (error (e)
      (log:error "encountered unrecoverable error" e))))
  
