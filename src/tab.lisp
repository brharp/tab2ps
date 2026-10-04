
(defparameter *page-width* 612)
(defparameter *page-height* 792)
(defparameter *page-margin-left* 16)
(defparameter *page-margin-right* 16)
(defparameter *page-margin-top* 24)
(defparameter *page-margin-bottom* 16)

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

(defstruct rect top left width height)

(defun rect-with (r &key top left width height)
  (make-rect :top (or top (rect-top r))
	     :left (or left (rect-left r))
	     :width (or width (rect-width r))
	     :height (or height (rect-height r))))

(defun measure-box (measure)
  (hbox (mapcar #'chord-box measure)))

(defun staff-box (staff)
  (hbox (mapcar #'measure-box staff)))

(defun render-frontmatter ()
  (format *ps-output* "%!PS
/Palatino-Roman 11.000000 selectfont
1.000000 setlinewidth
"))
;16.000000 776.000000 moveto
;"))

(defun render-hbox (box r)
  (let ((left (rect-left r)))
    (dolist (child (box-contents box))
      (let ((child-width (box-width child)))
	(render child (rect-with r :left left :width child-width))
	(incf left (* child-width 16)))))
  box)

(defun render-vbox (box r)
  (let ((top (rect-top r)))
    (dolist (child (box-contents box))
      (let ((child-height (box-height child)))
	(render child (rect-with r :top top :height child-height))
	(incf top (* child-height (- 16))))))
  box)

(defun render-text (box r)
  (format *ps-output* "~a ~a moveto~%" (rect-left r) (rect-top r))
  (format *ps-output* "(~a) show~%" (box-contents box))
  box)

(defun render (box r)
  (case (box-type box)
    (:hbox (render-hbox box r))
    (:vbox (render-vbox box r))
    (:text (render-text box r))))

(defun page-rect ()
  (make-rect :top (- *page-height* *page-margin-top*)
	     :left *page-margin-left*
	     :width (- *page-width* *page-margin-left* *page-margin-right*)
	     :height (- *page-height* *page-margin-top* *page-margin-bottom*)))

(defun main ()
  (render-frontmatter)
  (render (layout (transpose falls-of-richmond)) (page-rect))
  (format *ps-output* "showpage~%")
  (finish-output *ps-output*)
  t)

(main)
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

