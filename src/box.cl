
(require "posix" "/usr/local/lib/lisp/posix")

;; Boxes

(defclass box ()
  ((width :accessor box-width :initarg :width)))

;; Render

(defgeneric render (box))

(defmethod render ((b box))
  (format t ))


;; PS

(defvar *ps-output* t)

(defmacro with-ps (&body body)
  `(let ((*ps-output* (popen '("gs") "w")))
     ,@body
     (close *ps-output*)))

(defun ps-translate (x y)
  (format *ps-output* "~a ~a translate"
	  x y))

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
;;

(defun define-tabstaff ()
  (defps tabstaff () "
    % width
    /w exch def

    0 1 4 {
        /i exch def
        newpath
        0 i 12 mul moveto
        w i 12 mul lineto
        stroke
    } for"))

(with-ps
    (define-tabstaff)
  (ps-translate 10 10)
  (tabstaff)
  (ps-showpage))
