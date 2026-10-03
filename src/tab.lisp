
;; Falls of Richmond
(defvar falls-of-richmond
  '(( ( ( - - ) - - ( 0 - ) ) ( - ( - - ) ( - - - ) ( - - ) ) )
    ( ( ( - - ) - - ( 0 - ) ) ( - ( - 2 ) ( - - - ) ( - - ) ) )
    ( ( ( - - ) 0 0 ( - - ) ) ( - ( 0 - ) ( 3 2 0 ) ( - - ) ) )
    ( ( ( 0 3 ) - - ( - - ) ) ( 3 ( - - ) ( - - - ) ( 3 0 ) ) )
    ( ( ( - - ) - - ( - 0 ) ) ( - ( - - ) ( - - - ) ( - - ) ) )))

(defun matrixp (matrix)
  (and (listp matrix)
       (every #'listp matrix)))

(defun transpose-1 (tab)
  (cond ((null tab) ())
	((some #'null tab) ())
	(t (cons (mapcar #'car tab)
		 (transpose-1 (mapcar #'cdr tab))))))

(defun transpose (tab)
  (if (not (matrixp tab)) tab
      (mapcar #'transpose (transpose-1 tab))))

(transpose falls-of-richmond)

;; Transposing the staff gives us a list of measures
;; Transposing a measure gives a list of beats
(let* ((measures (transpose falls-of-richmond))
       (beats (transpose (first measures)))
       (half-beats (transpose (first beats))))
   (first half-beats))

(defstruct box
  type
  height
  width
  contents)

(defun hbox (boxes)
  (make-box :type :hbox
	    :width (apply #'+ (mapcar #'box-width boxes))
	    :height (apply #'max (mapcar #'box-height boxes))
	    :contents boxes))

(defun vbox (boxes)
  (make-box :type :vbox
	    :width (apply #'max (mapcar #'box-width boxes))
	    :height (apply #'+ (mapcar #'box-height boxes))
	    :contents boxes))

(defun text (string)
  (make-box :type :text
	    :width 1
	    :height 1
	    :contents string))

(defun layout (tab)
  (if (matrixp tab)
      (hbox (mapcar #'layout tab))
      (vbox (mapcar #'text tab))))

;;(layout (transpose falls-of-richmond))

(defun measure-box (measure)
  (hbox (mapcar #'chord-box measure)))

(defun staff-box (staff)
  (hbox (mapcar #'measure-box staff)))

(defun render-frontmatter ()
  (format t "%!PS
/Palatino-Roman 11.000000 selectfont
1.000000 setlinewidth
16.000000 776.000000 moveto"))

(defun gsave () (format t "gsave~%"))
(defun grestore () (format t "grestore~%"))
(defun moveto (x y) (format t "~a ~a moveto~%" x y))
(defun translate (x y) (format t "~a ~a translate~%" x y))

(defmacro with-gsave (&body body)
  `(progn
     (gsave)
     (unwind-protect
	  (progn ,@body)
       (grestore))))

(defun render-hbox (box)
  (with-gsave
      (do* ((c (box-contents box) (cdr c)))
	   ((null c) box)
	(let* ((b (car c)) (w (box-width b)))
	  (render b)
	  (translate w 0)))))  

(defun render-vbox (box)
  (with-gsave
      (do* ((c (box-contents box) (cdr c)))
	   ((null c) box)
	(let* ((b (car c)) (h (box-height b)))
	  (render b)
	  (translate 0 h)))))

(defun render-text (box)
  (prog1 box (format t "(~a) show~%" (box-contents box))))

(defun render (box)
  (case (box-type box)
    (:hbox (render-hbox box))
    (:vbox (render-vbox box))
    (:text (render-text box))))
  
(defun main ()
  (render (layout (transpose falls-of-richmond)))
  t)

(main)
