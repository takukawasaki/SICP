


(defun flipped-pairs (painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(defun right-split (painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
	(beside painter (below smaller smaller)))))

(defun corner-split (painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1)))))
	(beside (below painter top-left)
		(below bottom-right corner)))))

(defun square-limit (painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vart half) half))))


;;2-44
(defun up-split (painter n)
  (if (- n 0)
      painter
      (let ((up (up-split painter (- n 1))))
	(below painter (beside up up)))))


(defun square-of-four (tl tr bl br)
  (lambda (painter)
    (let ((top (beside (funcall tl painter) (funcall tr painter)))
	  (bottom (beside (funcall bl painter) (funcall br painter))))
      (below bottom top))))

(defun flipped-pairs (painter)
  (let ((combine4 (square-of-four #'identity #'flip-vert
				  #'identity #'flip-vert)))
    (funcall combine4 (corner-split painter n))))


(defun square-limit (painter n)
  (let ((combine4 (square-of-four #'flip-horiz #'identity
				  #'rotate180 #'flip-vert)))
    (funcall combine4 (corner-split painter n))))

(defun split (op1 op2)
  (lambda (painter n)
    (if (= n 0)
	painter
	(let ((smaller ((split #'op1 #'op2) painter (- n 1))))
	  (funcall #'op1 painter (funcall #'op2 smaller smaller))))))

(defun frame-coord-map (frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))


;;2-46
(defun make-vect (point1 point2)
  (cons point1 point2))

(defun xcor-vect (vector)
  (car vector))

(defun ycor-vect (vector)
  (cdr vector))

(defun add-vect (vector1 vector2)
  (make-vect (+ (xcor-vect vector1)
		(xcor-vect vector2))
	     (+ (ycor-vect vector1)
		(ycor-vect vector2))))

(defun sub-vect (vector1 vector2)
  (make-vect (- (xcor-vect vector1)
		(xcor-vect vector2))
	     (- (ycor-vect vector1)
		(ycor-vect vector2))))

(defun scale-vect (s v)
  (make-vect (* s (xcor-vect v))
	     (* s (ycor-vect v))))


;;2-47
(defun make-frame (origin edge1 edge2)
  (list origin edge1 edge2))

(defun origin-frame (frame)
  (car frame))

(defun edge1-frame (frame)
  (cadr frame))

(defun edge2-frame (frame)
  (caddr frame))

(defun make-frame2 (origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(defun origin-frame2 (frame)
  (car frame))
(defun edge1-frame2 (frame)
  (cadr frame))

(defun edge2-frame2 (frame)
  (caddr frame))


(defun segment->painter (segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))


;;2-48
(defun  make-segment (start end)
  (cons start end))

(defun start-segment (segment)
  (car segment))

(defun end-segment (segment)
  (cdr segment))



(defparameter outline
  (segment->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
	 (make-segment (make-vect 0 1) (make-vect 1 1))
	 (make-segment (make-vect 1 1) (make-vect 0 1))
	 (make-segment (make-vect 0 1) (make-vect 0 0)))))

(defparameter x-x
  (segment->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
	 (make-segment (make-vect 0 1) (make-vect 1 0)))))

(defparameter diamond-painter
  (segment->painter
   (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
	 (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
	 (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
	 (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(defparameter  wave
  (segment->painter
   (list (make-segment (make-vect 0.000 0.645) (make-vect 0.154 0.411))
	 (make-segment (make-vect 0.154 0.411) (make-vect 0.302 0.588))
	 (make-segment (make-vect 0.302 0.588) (make-vect 0.354 0.497))
	 (make-segment (make-vect 0.354 0.497) (make-vect 0.245 0.000))
	 (make-segment (make-vect 0.419 0.000) (make-vect 0.497 0.171))
	 (make-segment (make-vect 0.497 0.171) (make-vect 0.575 0.000))
	 (make-segment (make-vect 0.748 0.000) (make-vect 0.605 0.462))
	 (make-segment (make-vect 0.605 0.462) (make-vect 1.000 0.142))
	 (make-segment (make-vect 1.000 0.354) (make-vect 0.748 0.657))
	 (make-segment (make-vect 0.748 0.657) (make-vect 0.582 0.657))
	 (make-segment (make-vect 0.582 0.657) (make-vect 0.640 0.857))
	 (make-segment (make-vect 0.640 0.857) (make-vect 0.575 1.000))
	 (make-segment (make-vect 0.419 1.000) (make-vect 0.354 0.857))
	 (make-segment (make-vect 0.354 0.857) (make-vect 0.411 0.657))
	 (make-segment (make-vect 0.411 0.657) (make-vect 0.285 0.657))
	 (make-segment (make-vect 0.285 0.657) (make-vect 0.154 0.605))
	 (make-segment (make-vect 0.154 0.605) (make-vect 0.000 0.857)))))


(defun painter-> (x y)
  `(segment->painter
    (list (make-segment (make-vect ,x ,y) (make-vect ,x ,y)))))

(defun vect-> (a)
  (mapcar #'(lambda (x)
	      (make-segment (make-vect (car x) (second x)) (make-vect (third x) (fourth x)))) a))

(defmacro paint-> (L)
  `(segment->painter (list (vect-> ,L))))







(defun transform-painter (painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (funcall m origin)))
	(funcall painter
		 (make-frame new-origin
			     (sub-vect (funcall m corner1) new-origin)
			     (sub-vect (funcall m corner2) new-origin)))))))


(defun flip-vert (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(defun shrink-to-upper-right (painter)
  (transform-painter painter
		     (make-vect 0.5 0.5)
		     (make-vect 1.0 0.5)
		     (make-vect 0.5 1.0)))

(defun rotate90 (painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(defun squafh-inwards (painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))


(defun flip-horiz (painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(defun rotate180 (painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)))

(defun rotate270 (painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))




;;2-51

(defun below (painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-below
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      (make-vect 1.0 0.0)
			      split-point))
	  (paint-above
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.5)
			      (make-vect 0.0 1.0))))
      (lambda (frame)
	(paint-below frame)
	(paint-above frame)))))

(defun below (painter1 painter2)
  (rotate90
   (beside (rotate270 painter1)
	   (rotate270 painter2))))

;;2-52

					;a
(defvar wave
  (list (make-segment (make-vect 0.000 0.645) (make-vect 0.154 0.411))
	(make-segment (make-vect 0.154 0.411) (make-vect 0.302 0.588))
	(make-segment (make-vect 0.302 0.588) (make-vect 0.354 0.497))
	(make-segment (make-vect 0.354 0.497) (make-vect 0.245 0.000))
	(make-segment (make-vect 0.419 0.000) (make-vect 0.497 0.171))
	(make-segment (make-vect 0.497 0.171) (make-vect 0.575 0.000))
	(make-segment (make-vect 0.748 0.000) (make-vect 0.605 0.462))
	(make-segment (make-vect 0.605 0.462) (make-vect 1.000 0.142))
	(make-segment (make-vect 1.000 0.354) (make-vect 0.748 0.657))
	(make-segment (make-vect 0.748 0.657) (make-vect 0.582 0.657))
	(make-segment (make-vect 0.582 0.657) (make-vect 0.640 0.857))
	(make-segment (make-vect 0.640 0.857) (make-vect 0.575 1.000))
	(make-segment (make-vect 0.419 1.000) (make-vect 0.354 0.857))
	(make-segment (make-vect 0.354 0.857) (make-vect 0.411 0.657))
	(make-segment (make-vect 0.411 0.657) (make-vect 0.285 0.657))
	(make-segment (make-vect 0.285 0.657) (make-vect 0.154 0.605))
	(make-segment (make-vect 0.154 0.605) (make-vect 0.000 0.857))
	(make-segment (make-vect 0.428 0.828) (make-vect 0.471 0.840))
	(make-segment (make-vect 0.528 0.840) (make-vect 0.571 0.828))))



(defun corner-split (painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1)))
	    (corner (corner-split painter (- n 1))))
	(beside (below painter up)
		(below right corner)))))


(defun corner-split (painter n)
  (if (= n 0)
      (flip-horiz painter)
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(defun square-of-four (tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(defun square-limit (painter n)
  (let ((combine4 (square-of-four flip-horiz identity
				  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

(defun identity0 (x) x)

(defvar image->painter
  (lambda (frame)
    (let ((origin ((frame-coord-map frame) (make-vect 0 0)))
	  (edge1 ((frame-coord-map frame) (make-vect 1.0 0)))
	  (edge2 ((frame-coord-map frame) (make-vect 0 1.0))))
      (fresh-line)
      (princ (list (- (car edge1) (car origin))
		   (- (cdr edge1) (cdr origin))
		   (- (car edge2) (car origin))
		   (- (cdr edge2) (cdr origin))
		   (car origin) (cdr origin))))))



