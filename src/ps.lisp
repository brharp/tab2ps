
(require "posix" "/usr/local/lib/lisp/posix")

;; PS

(defvar *ps-output* t)

(defun ps-open ()
  (setq *ps-output* (popen '("gs") "w")))

(defun ps-close()
  (close *ps-output*)
  (setq *ps-output* t))

(defmacro with-ps (&body body)
  `(let ((*ps-output* (popen '("gs") "w")))
     ,@body
     (close *ps-output*)))

(defun gsave () (format *ps-output* "gsave~%"))
(defun grestore () (format *ps-output* "grestore~%"))
(defun moveto (x y) (format *ps-output* "~a ~a moveto~%" x y))
(defun translate (x y) (format *ps-output* "~a ~a translate~%" x y))
(defun erasepage () (format *ps-output* "erasepage~%"))
(defun showpage () (format *ps-output* "showpage~%") (finish-output *ps-output*))
(defun newpath () (format *ps-output* "newpath~%"))
(defun stroke () (format *ps-output* "stroke~%"))
(defun lineto (x y) (format *ps-output* "~a ~a lineto~%" x y))
(defun show (s) (format *ps-output* "(~a) show~%" s))

(defun ps-def (name body)
  (format *ps-output*
	  "/~a { ~a } def"
	  name body))

(defun ps-showpage ()
  (format *ps-output* "showpage"))

(defmacro defps (name args body)
  (let ((a (gensym)))
    `(progn
       (ps-def ,(symbol-name name) ,body)
       (defun ,name ,args
	 ,@(dolist (a args)
	     `(format *ps-output* "~a" ,a))
	 (format *ps-output* "~a" ,(symbol-name name))))))

(defmacro with-gsave (&body body)
  `(progn
     (gsave)
     (unwind-protect
	  (progn ,@body)
       (grestore))))

