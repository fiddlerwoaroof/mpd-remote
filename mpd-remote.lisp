;;;; mpd-remote.lisp

(in-package #:mpd-remote)

;;; "mpd-remote" goes here. Hacks and glory await!

(defun call-with-mpd-connection (host port cb)
  (usocket:with-client-socket (sock stream host port :element-type '(unsigned-byte 8))
    (let ((stream (flexi-streams:make-flexi-stream stream :external-format :utf8)))
      (assert (alexandria:starts-with-subseq "OK" (read-line stream)))
      (funcall cb sock stream))))

(define-condition mpd-error ()
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
	     (format stream "~a" (message condition)))))

(defmacro do-until ((end-condition &optional result) &body body)
  `(do () (,end-condition ,@(when result `(,result)))
     ,@body))

(defun get-response (stream)
  (let ((result (list))
	(done nil))
      (do-until (done (nreverse result))
	(let ((line (read-line stream)))
	  (format t "~&~s~%" line)
	  (if (string= line "OK")
	      (setf done t)
	      (multiple-value-bind (error-p suffix) (alexandria:starts-with-subseq "ACK" line :return-suffix t)
		(if error-p
		    (error 'mpd-error :message (serapeum:trim-whitespace suffix))
		    (push line result))))))))

(defmacro with-mpd-connection ((socket-sym stream-sym &optional (host "127.0.0.1") (port 6600)) &body body)
  `(call-with-mpd-connection ,host ,port
			     (lambda (,socket-sym ,stream-sym)
			       ,@body)))

(defclass mpd-command ()
  ())

(defgeneric render-mpd-command (command args))

(defgeneric send-command (stream command &key)
  (:method (stream command &rest r &key)
    (write-line (render-mpd-command command
				    r)
		stream)
    (finish-output stream)
    (get-response stream)))

(defclass current-song (mpd-command)
  ())

(defmacro mpd-progn ((stream) &body body)
  `(let ((*standard-output* ,stream))
     (write-line "command_list_ok_begin")
     (unwind-protect (progn ,@body)
       (write-line "command_list_end"))))

(defmethod render-mpd-command ((command (eql :current-song)) args)
  (declare (ignore command args))
  "currentsong")

(defmethod send-command :around (stream (command (eql :current-song)) &key)
  (declare (ignore stream))
  (mpd-remote.song:parse-song (call-next-method)))

(defmethod render-mpd-command ((command (eql :playlist-info)) args)
  (declare (ignore command))
  (format nil "playlistinfo ~{~a~^:~}" (getf args :slice)))

(defmethod send-command :around (stream (command (eql :playlist-info)) &key slice)
  (declare (ignore command stream))
  (mapcar #'mpd-remote.song:parse-song
	  (mpd-remote.song:split-songs (call-next-method))))

(defmethod render-mpd-command ((command (eql :play)) args)
  (declare (ignore command))
  "play")

(defmethod render-mpd-command ((command symbol) args)
  (declare (ignore args))
  (when (keywordp command)
    (string-downcase command)))
