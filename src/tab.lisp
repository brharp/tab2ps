
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

(defun staff (content)
  (make-box :type :staff
	    :width (box-width content)
	    :height (box-height content)
	    :contents content))

(defun layout (tab)
  (if (matrixp tab)
      (hbox (mapcar #'layout tab))
      (vbox (mapcar #'text tab))))

(defun layout-staff (tab)
  (staff (layout tab)))
	    

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

(defun render-hbox (box r)
  (let* ((left (rect-left r))
	 (children (box-contents box))
	 (child-count (length children))
	 (child-width (floor (/ (rect-width r) child-count))))
    (dolist (child children)
      (render child (rect-with r :left left :width child-width))
      (incf left child-width)))
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

(defun render-staff (box r)
  (dotimes (i 5)
    (let* ((x1 (rect-left r))
	   (x2 (+ x1 (rect-width r)))
	   (y  (- (rect-top r) (* 16 i))))
      (newpath)
      (moveto x1 y)
      (lineto x2 y)
      (stroke)))
  (render (box-contents box) r))

(defun render (box r)
  (case (box-type box)
    (:hbox (render-hbox box r))
    (:vbox (render-vbox box r))
    (:text (render-text box r))
    (:staff (render-staff box r))))

(defun page-rect ()
  (make-rect :top (- *page-height* *page-margin-top*)
	     :left *page-margin-left*
	     :width (- *page-width* *page-margin-left* *page-margin-right*)
	     :height (- *page-height* *page-margin-top* *page-margin-bottom*)))

(defun render-score ()
  (render-frontmatter)
  (render (layout-staff (transpose falls-of-richmond)) (page-rect))
  (showpage))

(defun main ()
  (with-ps (render-score)
    (y-or-n-p))
  t)

;;
