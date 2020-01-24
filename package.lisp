;;;; package.lisp

(defpackage #:instance-emoji-bot
  (:use #:cl #:glacier)
  (:import-from :json
		:decode-json-from-string))
