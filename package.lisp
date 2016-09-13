;;;; package.lisp

(defpackage #:mpd-remote.song
  (:use #:cl #:alexandria #:serapeum)
  (:export #:parse-song #:split-songs))

(defpackage #:mpd-remote
  (:use #:cl #:alexandria #:serapeum #:mpd-remote.song)
  (:export #:with-mpd-connection #:send-command))

