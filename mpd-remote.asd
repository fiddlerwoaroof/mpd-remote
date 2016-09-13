;;;; mpd-remote.asd

(asdf:defsystem #:mpd-remote
  :description "Describe mpd-remote here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:fwoar.lisputils
               #:alexandria
	       #:serapeum
	       #:flexi-streams
	       #:usocket)
               
  :serial t
  :components ((:file "package")
               (:file "song-handling")
               (:file "mpd-remote")))

