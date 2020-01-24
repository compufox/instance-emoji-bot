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
      (setf (tooter:content status)
	    (str:replace-all (bot-username glacier::*bot*) "" (tooter:content status)))
      
      (let ((domain (cl-ppcre:scan-to-strings "(?:[^./]+[.])*([^/.]+[.][^/.]+)" (tooter:content status))))
	(when (and domain
		   (not (member domain *blacklist* :test #'string=)))
	  (push domain *known-instances*)
	  (download-emoji-list domain)

	  (let ((emoji (choose-emoji)))
	    (reply status (format nil "~a from ~a" (agetf emoji :shortcode) domain)
		   :media (download-emoji emoji))))))))

(defun load-instance-list ()
  (setf *known-instances* (uiop:read-file-lines #P"instance.list")))

(defun write-instance-list ()
  (str:to-file #P"instance.list" (str:join (string #\Newline) *known-instances*)))

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
  (run-bot (make-instance 'mastodon-bot :config-file "bot.config"
					:on-notification #'parse-reply)
    (after-every (1 :hour :async t) (update-emojis))
    (after-every (1 :hour)
      (let ((emoji (choose-emoji)))
	(post (format nil "~a from ~a" (agetf emoji :shortcode) domain)
	      :cw "emoji"
	      :media (download-emoji emoji))))))
