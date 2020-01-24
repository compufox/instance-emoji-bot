;;;; instance-emoji-bot.asd

(asdf:defsystem #:instance-emoji-bot
  :description "Describe instance-emoji-bot here"
  :author "ava fox"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:glacier #:cl-ppcre #:dexador #:cl-json #:str)
  :components ((:file "package")
               (:file "instance-emoji-bot")))
