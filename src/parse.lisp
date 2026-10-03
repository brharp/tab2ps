
(defun read-staff (nlines stream)
  (multiple-value-bind (notes beams measures)
      (read-rhythm-line)
    (let ((pitches (make-array nlines (length notes))))
      (read-melody pitches stream))
    (make-staff :notes notes
		:beams beams
		:measures measures
		:pitches pitches)))

(defun parse-rhythm-line ()
  (let* ((notes    ())
	 (beams    ())
	 (measure  (make-measure))
	 (measures (list measure)))
    (case c
      ((#\[) (setq b (make-beam)))
      ((#\]) (when b (push b beams) (setq b nil))))
    (push n notes)
    (push n measure)
    (when b (push n beam))))

(defun parse-tab (input)
  (flet ((next (input notes beams measures)
	   (let ((measure (first measures))
		 (beam    (first beams)))
	     (cond
	       ;; end of input
	       ((null input)
		(make-tab :rhythm (make-rhythm :notes notes
					       :beams beams)
			  :melody melody))
	       ;; open beam 
	       ((eq (caar input) :open-beam)
		(next (rest input) notes (cons (make-beam) beams) measures))
	       ;; close beam
	       ((eq (caar in) :close-beam)
		(next (rest input) notes (cons nil beams) measures))
	       ;; break measure
	       ((eq (caar in) :bar-line)
		(next (rest input) notes beams (cons (make-measure) measures)))
	       ;; duration
	       ((eq (caar input) :duration)
		(let (note (make-note :duration (cdar input)))
		  (next (rest input)
			(cons note notes)
			(cons (cons note measure) (rest measures))
			(if (null beam) beams
			    (cons (cons note beam) (rest beams))))))))))))

;; Break down each measure into intervals.
q[ee] q[ee]
( ( q ( e e ) ) ( q ( e e ) ) )

;; Falls of Richmond
( ( ( - - ) - - ( 0 - ) ) ( - ( - - ) ( - - - ) ( - - ) ) )
( ( ( - - ) - - ( 0 - ) ) ( - ( - 2 ) ( - - - ) ( - - ) ) )
( ( ( - - ) 0 0 ( - - ) ) ( - ( 0 - ) ( 3 2 0 ) ( - - ) ) )
( ( ( 0 3 ) - - ( - - ) ) ( 3 ( - - ) ( - - - ) ( 3 0 ) ) )
( ( ( - - ) - - ( - 0 ) ) ( - ( - - ) ( - - - ) ( - - ) ) )

;; == transform ==>

( ( ( - - - 0 - )
    ( - - - 3 - ) )
  ( - - 0 - - )
  ( - - 0 - - )
  ( ( 0 0 - - - )
    ( - - - - 0 ) ) )

(defun transpose (lines)
  


;; Assuming 4/4 time, a measure of length 4 assigns quarter note duration to
;; each element of the list.


