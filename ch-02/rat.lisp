(ma)


(defun add-rat (x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x)(denom y))))

(defun subrat (x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(defun mul-rat (x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(defun div-rat (x y)
  (make-rat (* (numer x) (denom y))
	    (* (denom x) (numer y))))

(defun equal-rat? (x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(defun numer (x)
  (car x))

(defun denom (x)
  (cdr x))

(defun print-rat (x)
  (fresh-line)
  (princ (numer x))
  (princ "/")
  (princ (denom x)))

(defun gcds (a b)
  (if (= b 0)
      a
      (gcd b (mod a  b))))

;;2-1
(defun make-rat (n d)
  (let ((g (abs (gcds n d))))
    (if (< d 0)
	(cons (/ (- n) g) (/ (- d) g))
	(cons (/ n g) (/ d g)))))


;;2-2
(defun make-segment (x y)
  (cons x y))

(defun start-segment (x)
  (car x))


(defun end-segment (x)
  (cdr x))

(defun make-point (xp yp)
  (cons xp yp))

(defun x-point (point)
  (car point))

(defun y-point (point)
  (cdr point))

(defun midpoint-segment (segment)
  (make-point (/ (+ (x-point (start-segment segment))
		    (x-point (end-segment segment)))
		 2)
	      (/ (+ (y-point (start-segment segment))
		    (y-point (end-segment segment)))
		 2)))

(defun print-point (p)
  (fresh-line)
  (princ "(")
  (princ (x-point p))
  (princ ",")
  (princ (y-point p))
  (princ ")"))

;;2-3

(defun make-rec (x y)
  (cons x y))

(defun corner0-rec (rec)
  (car rec))

(defun corner1-rec (rec)
  (cdr rec))


(defun side1 (rec)
  (abs (- (x-point (corner0-rec rec))
	  (x-point (corner1-rec rec)))))
(defun side2 (rec)
  (abs (- (y-point (corner0-rec rec))
	  (y-point (corner1-rec rec)))))

(defun perimeter (rec)
  (* (+ (side1 rec)
	(side2 rec))
     2))

(defun area (rec)
  (* (side1 rec) (side2 rec)))

(defun cons00 (x y)
  (labels ((dispatch (m)
	     (cond ((= m 0) x)
		   ((= m 1) y)
		   (t (error "Argument not 0 or 1 -- CONS" )))))
    #'dispatch))

(defun car1 (z)
  (funcall z 0))

(defun cdr1 (z)
  (funcall z 1))

;;2-4

(defun cons01 (x y)
  (lambda (m) (funcall m x y)))

(defun car2 (z)
  (funcall z #'(lambda (p q) p)))

(defun cdr2 (z)
  (funcall z #'(lambda (p q) q)))




(defun ss ()
  ((lambda (a p) p) 1 2))

;;2-5
(defun fast-expt (b n)
  (cond ((= n 0) 1)
	((evenp n) (square (fast-expt b (/ n 2))))
	(t (* b (fast-expt b (- n 1))))))

(defun square (x)
  (* x x))

(defun cons-expt (a b)
  (* (fast-expt 2 a) (fast-expt 3 b)))

(defun acar (x)
  (if (= (mod x 2) 0)
      (+ 1 (acar (/ x 2)))
      0))

(defun bcdr (x)
  (if (= (mod x 3) 0)
      (+ 1 (bcdr (/ x 3)))
      0))

;;2-6

(defvar zero (lambda (f) (lambda (x) x)))

(defun add-1 (n)
  (lambda (f) (lambda (x) (funcall f (funcall (funcall n f) x)))))


(defvar one (lambda (f) (lambda (x) (funcall f x))))
(defvar two (lambda (f) (lambda (x) (funcall f (funcall f x)))))

(defun church+ (m n)
  (lambda (f)
    (lambda (x)
      (funcall (funcall m  f) (funcall (funcall n f) x)))))


(defun inc (n) (+ n 1))

(defvar  four (funcall #'church+ two two))


;; (funcall (funcall (funcall #'add-1 zero) #'inc) 0)
;;  (funcall (funcall two #'inc) 0)
;; (funcall (funcall four #'inc) 0)


;;2-1-4

(defun add-interval (x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
		 (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
	(p2 (* (lower-bound x) (upper-bound y)))
	(p3 (* (upper-bound x) (lower-bound y)))
	(p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(defun div-interval (x y)
  (mul-interval x
		(make-interval (/ 1.0 (upper-bound y))
			       (/ 1.0 (lower-bound y)))))

;;2-7
(defun make-interval (x y)
  (cons x y))

(defun lower-bound (x)
  (car x))

(defun upper-bound (x)
  (cdr x))


;;2-8
(defun sub-interval (x y)
  (make-interval (- (lower-bound x) (upper-bound y))
		 (- (upper-bound x) (lower-bound y))))

;;2-9
(defun width (x)
  (/ (- (cdr x) (car x)) 2))

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
	  (t
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
;;(percent a) ==> 1
;;(percent b) ==> 1
(defvar q0 (div-interval a a))
;;(percent q0) ==> 1.9999500012499796  ;1%どうしを割って2$の誤差になった.
(defvar q1 (div-interval a b))
;;(percent q1) ==> 1.9999500012499796  ;同様

;;1を定義して抵抗の計算みたいなことをやってみる.

(defvar one (make-center-percent 1 0))

(defvar p (mul-interval a b))
(defvar s (add-interval a b))
(percent p) ;;==>  80000/40001
;;(exact->inexact (percent p)) ;;==> 1.9999500012499687  ;積の誤差は2%
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

;;(defvar p1 (par1 r1 r2)) ;par1で計算した結果をp1
;;(defvar p2 (par2 r1 r2)) ;par2で計算した結果をp2とする 正確な並列抵抗は120

;;p1 ==> (111.29268292682927 . 129.30769230769232)

;;p2 ==> (116.99999999999999 . 123.00000000000001)

(center p1) ;;==> 120.3001876172608

(percent p1) ;;==> 14.975046787273868

(center p2) ;;==> 120.

(percent p2) ;;==> 5.000000000000024
