;;;; package.lisp

(defpackage #:instance-emoji-bot
  (:use #:cl #:glacier #:with-user-abort)
  (:import-from :json
		:decode-json-from-string))
