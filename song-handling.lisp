(in-package #:mpd-remote.song)

(defclass song ()
  ((tags :accessor tags :initform (make-hash-table :test 'equalp))))

(defmacro define-tag-accessor (name class &optional (tag name))
  `(defgeneric ,name (song)
     (:method ((song ,class))
       (gethash ,(string tag)
		(tags song)))))

(defmacro define-tag-accessors ((class) &body tags)
  `(progn
     ,@(loop for (name tag) in (fw.lu:ensure-mapping tags)
	  collect `(define-tag-accessor ,name ,class,tag))))



(define-tag-accessors (song)
  file artist album title genre last-modified track date (track-length time) pos)

(defmethod print-object ((song song) stream)
  (print-unreadable-object (song stream :type t :identity t)
    (with-accessors ((tags tags)) song
      (format stream "(~a - ~a)"
	      (gethash "title" tags)
	      (gethash "artist" tags)))))

(defun parse-tag (tag value)
  (string-case:string-case ((string-downcase tag) :default value)
    ("track" (mapcar #'parse-integer
		     (split-sequence #\/ value)))
    ("disc" (mapcar #'parse-integer
		    (split-sequence #\/ value)))
    ("time" (parse-integer value))  
    ("pos" (parse-integer value))
    ("date" (parse-integer value))))

(defun parse-song (lines)
  (let ((song (make-instance 'song)))
    (dolist (line lines song)
      (destructuring-bind (tag value) (split-sequence #\: line :count 2)
	(setf (gethash tag (tags song))
	      (parse-tag tag (trim-whitespace value)))))))

(defun split-songs (lines)
  (let* ((result (list (list))))
    (dolist (line lines (mapcar #'nreverse (cdr (nreverse result))))
      (when (starts-with-subseq "file:" line)
	(push (list) result))
      (push line (car result)))))
				
	 
(defun parse-search (lines)
  (declare (optimize (debug 3)))
  (loop with result = (make-array 0 :adjustable t :fill-pointer 0)
     with current = (list)
     for line in lines
     for (tag value) = (split-sequence #\: line)

     when (string-equal tag "file") do
       (vector-push-extend current result)
       (setf current (list))

     do (push (vector tag (string-trim '(#\space #\tab) value))
	      current)
     finally (return result)))
