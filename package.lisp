;;;; package.lisp

(defpackage #:instance-emoji-bot
  (:use #:cl #:glacier #:with-user-abort)
  (:import-from :unix-opts
		:define-opts
		:get-opts)
  (:import-from :json
		:decode-json-from-string))
(in-package #:instance-emoji-bot)

(define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :config
   :description "bot config file to load"
   :short #\c
   :long "config"
   :arg-parser #'identity
   :meta-var "FILE")
  (:name :log
   :description "print log info"
   :short #\l
   :long "log"))
