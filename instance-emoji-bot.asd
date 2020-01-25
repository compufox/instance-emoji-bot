;;;; instance-emoji-bot.asd

(asdf:defsystem #:instance-emoji-bot
  :description "mastodon bot that posts random custom emojis from various instances"
  :author "ava fox"
  :license  "nplv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:glacier #:cl-ppcre #:dexador
	       #:cl-json #:str #:with-user-abort
	       #:unix-opts #:log4cl)
  :components ((:file "package")
               (:file "instance-emoji-bot"))
  :build-operation "program-op"
  :build-pathname "bin/instance-emoji-bot"
  :entry-point "instance-emoji-bot::main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
