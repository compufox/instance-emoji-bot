;;;; instance-emoji-bot.lisp

(in-package #:instance-emoji-bot)

(defvar *emoji-list* nil)
(defvar *known-instances*
  (list "social.computerfox.xyz" "glaceon.social" "queer.party"
	"vulpine.club" "botsin.space" "cybre.space" "yiff.life"
	"donphan.social" "sleeping.town" "chomp.life"
	"computerfairi.es" "sanguine.space")
  "a default list of known instances")
(defvar *blacklist* nil
  "domain blacklist")

(defun parse-reply (notification)
  (when (mention-p notification)
    (let ((status (tooter:status notification)))

      ;; strips the bot's username out of the status
      (setf (tooter:content status)
	    (str:replace-all (bot-username glacier::*bot*) "" (tooter:content status)))

      ;; scans the post for something that looks like a domain name
      (let ((domain (cl-ppcre:scan-to-strings "(?:[^./]+[.])*([^/.]+[.][^/.]+)" (tooter:content status))))
	(when (and domain
		   (not (member domain *blacklist* :test #'string=)))
	  (push domain *known-instances*)
	  (download-emoji-list domain)
	  (write-instance-list)
	  
	  (let ((emoji (choose-emoji)))
	    (reply status (format nil "~a from ~a" (agetf emoji :shortcode) domain)
		   :media (download-emoji emoji))))))))

(defun block-domain (status)
  ;; strips the bot's username out of the status
  (setf (tooter:content status)
	(str:replace-all (bot-username glacier::*bot*) "" (tooter:content status)))

  ;; scans the post for something that looks like a domain name
  (let ((domain (cl-ppcre:scan-to-strings "(?:[^./]+[.])*([^/.]+[.][^/.]+)" (tooter:content status))))
    (push domain *blacklist*)
    (write-blacklist)))

(defun load-instance-list ()
  (setf *known-instances* (uiop:read-file-lines #P"instance.list")))

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

(defun choose-emoji ()
  (let ((emojis (decode-json-from-string (load-emoji-file (random-from-list *known-instances*)))))
    (random-from-list emojis)))

(defun download-emoji (alist)
  (let ((filename (merge-pathnames (concatenate 'string
						(agetf alist :shortcode)
						"."
						(pathname-type (agetf alist :url)))
				   (uiop:temporary-directory))))
    (handler-case
	(prog1 filename
	  (dex:fetch (agetf alist :url) filename))
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
      nil)
    (error (e)
      nil)))

(defun main ()
  (load-instance-list)
  (add-command "block" #'block-domain :privileged t)
  (run-bot (make-instance 'mastodon-bot :config-file "bot.config"
					:on-notification #'parse-reply)
    (after-every (2 :hours :async t) (update-emojis))
    (after-every (1 :day :async t) (clean-downloads))
    (after-every (1 :hour)
      (let ((emoji (choose-emoji)))
	(post (format nil "~a from ~a" (agetf emoji :shortcode) domain)
	      :cw "emoji"
	      :sensitive t
	      :media (download-emoji emoji))))))
