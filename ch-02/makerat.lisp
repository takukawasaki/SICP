(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun sub-rat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(defun equal-rat-p (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defun gdc (x y)
  (if (= y 0)
      x
      (gdc y (mod x y))))

(defun make-rat (n d)
  (let ((g (abs (gdc n d))))
    (if (< d 0)
	(cons (/ (- n) g) (/ (- d) g))
	(cons (/ n g) (/ d g)))))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun print-rat (x)
  (fresh-line)
  (princ (numer x))
  (princ "/")
  (princ (denom x))
  nil)

(defvar one-half (make-rat 1 2))
(defvar one-third (make-rat 1 3))

(defun x-point (x)
  (car x))

(defun y-point (x)
  (cdr x))

(defun make-point (n d)
  (cons n d))

(defun start-segment (x)
  (car x))

(defun end-segment (x)
  (cdr x))

(defun make-segment (x y)
  (cons x y))

(defun midpoint-segment (x)
  (make-point
   (/ (+(x-point (start-segment x))
	(x-point (end-segment x)))
      2)
   (/ (+ (y-point (start-segment x))
	 (y-point (end-segment x)))
      2)))

(defun print-point (p)
  (fresh-line)
  (princ "(")
  (princ (x-point p))
  (princ ",")
  (princ (y-point p))
  (princ ")"))

(defun make-rec (x y)
  (cons x y))

(defun side0 (rec) (car rec))
(defun side1 (rec) (cdr rec))

(defun perimeter (rec)
  (* (+ (side0 rec)
	(side1 rec))
     2))
(defun area (rec) (* (side0 rec)
		      (side1 rec)))

(defvar rec0 (make-rec 30 40))

(defun make-rectangul (corner0 corner1)
  (cons corner0 corner1))

(defun corner0 (rec) (car rec))
(defun corner1 (rec) (cdr rec))

(defun side0 (rec) (abs (-(x-point (corner0 rec))
			   (x-point (corner1 rec)))))
(defun side1 (rec) (abs (- (y-point (corner0 rec))
			    (y-point (corner1 rec)))))

(defun perimeter (rec) (* (+ (side0 rec) (side1 rec)) 2))
(defun area (rec) (* (side0 rec) (side1 rec)))
  




(defun make-rectangul (corner0 corner1)
  (cons corner0 corner1))

(defun corner0 (rec) (car rec))
(defun corner1 (rec) (cdr rec))

(defun side0 (rec) (abs (- (x-point (corner0 rec))
			   (x-point (corner1 rec)))))
(defun side1 (rec) (abs (- (y-point (corner0 rec))
			    (y-point (corner1 rec)))))

(defun perimeter (rec) (* (+ (side0 rec) (side1 rec)) 2))
(defun area (rec) (* (side0 rec) (side1 rec)))
  

(defun cons1 (x y)
  (defun (dispatch m)
    (cond ((= m 0) x)
	  ((= m 1) y)
	  (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(defun car1 (z) (z 0))
(defun cdr1 (z) (z 1))

;;2-4
(defun fcons (x y)
  (lambda (m) (funcall  m x y)))

(defun fcar (z)
  (funcall z (lambda (p q) p)))

(defun fcdr (z)
  (funcall z (lambda (p q) q)))

;;2-5
(defun fast-expt (b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))

(defun even? (n)
  (= (remainder n 2) 0))

(defun square (x)
  (* x x))

(defun cons-expt (a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))


(defun acar (z)
  (if (= (remainder z 2) 0)
      (+ 1 (acar (/ z 2) ))
      0))

(defun acdr (z)
  (if (= (remainder z 3) 0)
      (+ 1 (acdr (/ z 3)))
      0))


;;2-6
(defvar zero #'(lambda (f)
		 #'(lambda (x) x)))

(defun add-1 (n)
  (lambda (f)
    (lambda (x)
      (funcall f (funcall (funcall n f) x)))))

(defvar one #'(lambda (f)
	      #'(lambda (x)
		(funcall f x))))

(defvar two #'(lambda (f)
	      #'(lambda (x)
		  (funcall f (funcall f x)))))

(defun church+ (m n)
  #'(lambda (f)
    #'(lambda (x)
	(funcall (funcall m f) (funcall (funcall n f) x)))))

(defun inc (n) (+ n 1))

(defvar four (funcall #'church+ two two))

(defun inc (x) (+ x 1))

;; (funcall (funcall (funcall #'add-1 zero) #'inc) 0)
;;  (funcall (funcall two #'inc) 0)
;; (funcall (funcall four #'inc) 0)

(defun list-search (L k)
  (position k L))


(defun insert-after (L index new-value)
  (push new-value (rest (nthcdr index L)))
  L)


(defun delete-after (L x)
  (remove x L))

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (lower-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;2-7
(defun make-interval (a b)
  (cons a b))

(defun upper-bound (x)
  (cdr x))

(defun lower-bound (x)
  (car x))

;;2-8
(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))


;;2-9
(defun width (x)
  (/ (- (cdr x) (car x)) 2))

;;
;;
;;(defvar a (make-interval 9 11))
;;(width a)
;;(defvar b (make-interval 1 3))
;;(width b) 
;;(defun  c (make-interval 19 21))
;;(width c)
;;(width (mul-interval a b)) 
;;(width (mul-interval a c)) 
;;(width (div-interval a b)) 


;;2-10
(defun new-mul-interval (x y)
  (let ((xl (lower-bound x)) (xu (upper-bound x))
	(yl (lower-bound y)) (yu (upper-bound y)))
    (cond ((< xu 0)
	   (cond ((< yu 0) (make-interval (* xu yu) (* xl yl)))
		 ((< yl 0) (make-interval (* xl yu) (* xl yl)))
		 (t     (make-interval (* xl yu) (* xu yl)))))
	  ((< xl 0)
	   (cond ((< yu 0) (make-interval (* xu yl) (* xl yl)))
		 ((< yl 0) (make-interval (min (* xl yu) (* xu yl))
					  (max (* xl yl) (* xu yu))))
		 (t     (make-interval (* xl yu) (* xu yu)))))
	  (else
	   (cond ((< yu 0) (make-interval (* xu yl) (* xl yu)))
		 ((< yl 0) (make-interval (* xu yl) (* xu yu)))
		 (t     (make-interval (* xl yl) (* xu yu))))))))


(defun new-div-interval (x y)
  (let ((yl (lower-bound y)) (yu (upper-bound y)))
    (if (and (< yl 0) (< 0 yu))
	(error "divisor interval spans zero" yl yu)
	(mul-interval x
		      (make-interval (/ 1.0 yu) (/ 1.0 yl))))))


;;2-11

(defvar xp (make-interval 2 3))
(defvar yp (make-interval 4 5))
(defvar xm (make-interval -5 -4))
(defvar ym (make-interval -3 -2))
(defvar xz (make-interval -2 1))
(defvar yz (make-interval -1 2))



(princ (new-mul-interval xp yp))
(princ (mul-interval xp yp)) (fresh-line)
(princ (new-mul-interval xp yz))
(princ (mul-interval xp yz)) (fresh-line)
(princ (new-mul-interval xp ym))
(princ (mul-interval xp ym)) (fresh-line)
(princ (new-mul-interval xz yp))
(princ (mul-interval xz yp)) (fresh-line)
(princ (new-mul-interval xz yz))
(princ (mul-interval xz yz)) (fresh-line)
(princ (new-mul-interval xz ym))
(princ (mul-interval xz ym)) (fresh-line)
(princ (new-mul-interval xm yp))
(princ (mul-interval xm yp)) (fresh-line)
(princ (new-mul-interval xm yz))
(princ (mul-interval xm yz)) (fresh-line)
(princ (new-mul-interval xm ym))
(princ (mul-interval xm ym)) (fresh-line)


;;2-12
(defun make-center-percent (c p)
  (let ((w (/ (* c (/ p 100)) 2)))
    (make-center-width c w)))

(defun percent (i)
  (* (/ (- (upper-bound i) (lower-bound i)) (center i)) 100))

(defun make-center-width (c w)
  (make-interval (- c w) (+ c w)))

(defun make-center-percent (c p)
  (let ((w (/ (* c (/ p 100)) 2)))
    (make-center-width c w)))

(defun percent (i)
  (* (/ (- (upper-bound i) (lower-bound i)) (center i)) 100))

(defun center (i)
  (/ (+ (upper-bound i) (lower-bound i)) 2))

;;2-14

(defvar a (make-center-percent 100 1))
(defvar b (make-center-percent 200 1))
(percent a) ==> 1
(percent b) ==> 1
(defvar q0 (div-interval a a))
(percent q0) ==> 1.9999500012499796  ;1%どうしを割って2$の誤差になった.
(defvar q1 (div-interval a b))
(percent q1) ==> 1.9999500012499796  ;同様

;;1を定義して抵抗の計算みたいなことをやってみる.

(defvar one (make-center-percent 1 0))

(defvar p (mul-interval a b))
(defvar s (add-interval a b))
(percent p) ;;==>  80000/40001
(exact->inexact (percent p)) ;;==> 1.9999500012499687  ;積の誤差は2%
(percent s) ;;==> 1                                    ;和の誤差は1%
(percent (div-interval p s)) ;;==> 2.999800014998875   ;並列抵抗の誤差は3%

(defvar r0 (div-interval one a))
(percent r0) ;;==> 1.0000000000000036     ;1/aの誤差は1%
(defvar r1 (div-interval one b))
(percent r1) ;;==> 1.0000000000000036     ;1/bの誤差は1%
(defvar s0 (add-interval r0 r1))
(percent s0) ;; ==>  .9999999999999978     ;分母の誤差は1%
(defvar  r (div-interval one s0))
(percent r)   ;;==>.9999999999999858     ;並列抵抗の誤差は1%


;;2-15
(defvar r1 (make-center-percent 200 5))
(defvar r2 (make-center-percent 300 5))

(defvar p1 (par1 r1 r2)) ;par1で計算した結果をp1
(defvar p2 (par2 r1 r2)) ;par2で計算した結果をp2とする 正確な並列抵抗は120

;;p1 ==> (111.29268292682927 . 129.30769230769232)

;;p2 ==> (116.99999999999999 . 123.00000000000001)

(center p1) ;;==> 120.3001876172608

(percent p1) ;;==> 14.975046787273868

(center p2) ;;==> 120.

(percent p2) ;;==> 5.000000000000024
