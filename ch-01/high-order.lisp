 (defun average (x y)
  (/ (+ x y) 2))



(defun cube (x)
  (* x x x))

(defun sum-integers (a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(defun sum-cube (a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cube (+ a 1) b))))

(defun pi-sum (a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(defun sum (a term next b)
  (if (> a b)
      0
      (+ (funcall term a)
	 (sum (funcall next a) term next b))))

(defun square (x)
  (* x x))


;;letの場合定義された関数を引数にする場合#'は必要ない
(defun pi-sum2 (a b)
  (let ((pi-term (lambda (x) (/ 1.0 (* x (+ x 2)))))
	(next (lambda (x) (+ x 4))))
    (sum a pi-term next b)))



;;labelの場合、関数のなかで引数に　関数に渡すときは#'が必要になる
(defun integral (f a b dx)
  (labels ((add-dx (x) (+ x dx)))
    (* dx
       (sum  (+ a (/ dx 2)) f #'add-dx b))))


(defun simpson (f a b n)
  (let* ((h (/ (- b a) n))
	 (next (lambda (x) (+ x 2)))
	 (y (lambda (k) (funcall f (+ a (* k h))))))
    (* (/ h 3)
       (+ (funcall y 0)
	  (* 4 (sum 1 y next (- n 1)))
	  (* 2 (sum 2 y next (- n 2)))
	  (funcall y n)))))

(defun simpson02 (f a b n)
  (let ((h (/ (- b a) n))
	(next (lambda (x) (+ x 2))))
    (labels ((y (k)
	       (funcall f (+ a (* k h)))))
      (* (/ h 3)
	 (+ (y 0)
	    (* 4 (sum 1 #'y next (- n 1)))
	    (* 2 (sum 2 #'y next (- n 2)))
	    (y n))))))


(defun product (a term next b)
  (if (> a b)
      1
      (* (funcall term a)
	 (product (funcall next a) term next b))))
(defun plus1 (x)
  (+ x 1))


(defun identityies (x)
  x)

(defun fact (n)
  (product 1 #'identityies #'plus1 n))

(defun pi-facter (n)
  (/ (* (- n 1)(+ n 1))
     (* n n)))

(defun pi-fact-next (n)
  (+ n 2))
  

(defconstant pi-mine
  (*  (product 3.0 #'pi-facter #'pi-fact-next  1000) 4))


(defun accumulate (combiner null-value term a next b)
  (if (> a b)
      null-value
      (funcall combiner (funcall term a)
		(accumulate combiner null-value term (funcall next a) next b))))

(defun accumulate-iter (combiner null-value term a next b)
  (flet ((accum-iter (a result)
	     (if (> a b)
		 result
		 (accum-iter (funcall next a)
			     (funcall combiner (funcall term a) result)))))
    (accum-iter a null-value)))


(defun fact2 (n)
  (accumulate-iter #'* 1 #'identityies 1 #'plus1 n))

(defun search00 (f neg-point pos-point)
  (let ((mid-point  (average neg-point pos-point)))
    (if (close-enough neg-point pos-point)
	mid-point
	(let ((test-value (funcall f mid-point)))
	  (cond ((plusp test-value)
		 (search00 f neg-point mid-point))
		((minusp test-value)
		 (search00 f mid-point pos-point))
		(t mid-point))))))

(defun close-enough (x y)
  (< (abs (- x y)) 0.001))

(defun half-internal-method (f a b)
  (let ((a-value (funcall f a))
	(b-value (funcall f b)))
    (cond ((and (minusp a-value) (plusp b-value))
	   (search00 f a b))
	  ((and (plusp a-value) (minusp b-value))
	   (search00 f b a))
	  (t
	   (error "error")))))



(defparameter  tolerlance 0.00001)


(defun fixed-point (f first-guess)
  (labels ((close-enough (v1 v2)
	     (< (abs (- v1 v2)) tolerlance))
	   (try (guess)
	     (let ((next (funcall f guess)))
	       (princ next)
	       (fresh-line)
	       (if (close-enough guess next)
		   next
		   (try next)))))
    (try first-guess)))

(defun sqrt00 (x)
  (fixed-point (lambda (y) (average y (/ x y)))
	       1.0))

(defun natural ()
  (fixed-point (lambda (x)(/ (log 1000) (log x)))
	       2.0))

(defun cont-frac (n d k)
  (labels ((rec (i)
	   (if (= i k)
	       (/ (funcall n i) (funcall d i))
	       (/ (funcall n i) (+ (funcall d i) (rec (+ i 1)))))))
    (rec 1.0)))


(defun cont-frac-iter (n d k)
  (labels ((rec (i result)
	     (if (= i 0)
		 result
		 (rec (decf i)(/ (funcall n i) (+ (funcall d i) result))))))
    (rec (decf k) (/ (funcall n k)(funcall d k)))))

(defun average-damp (f)
  (lambda (x) (average x (funcall f x))))


(defun sqrt01 (x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
	       1.0))

(defparameter dx 0.00001)



(defun deriev (g)
  (lambda (x)
    (/ (- (funcall g (+ x dx)) (funcall g x))
		 dx)))


(defun newton-transform (g)
  (lambda (x)
    (- x (/ (funcall g x) (funcall (deriev g) x)))))

(defun newton-method (g guess)
  (fixed-point (newton-transform g) guess))

(defun sqrt02 (x)
  (newton-method (lambda (y) (- (square y) x))
		 1.0))

(defun fixed-point-of-transform (g transform guess)
  (fixed-point (funcall transform g) guess))

(defun sqrt03 (x)
  (fixed-point-of-transform (lambda (y) (/ x y))
			 #'average-damp
			 1.0))


(defun sqrt04 (x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
			    #'newton-transform
			    1.0))
(defun cubic (a b c)
  #'(lambda (x)
      (+ (cube x)
	 (* a (square x))
	 (* b x)
	 c)))
(defun doubles (f)
  #'(lambda (x)
   (funcall f (funcall f x))))

(defun compose (f g)
  #'(lambda (x)
    (funcall f (funcall g x))))


(defun repeated (f n)
    (if (= n 0)
	#'identityies
	(compose f (repeated f (- n 1)))))

(defun smooth (f)
  #'(lambda (x)
      (/ (+ (funcall f (- x dx))
	    (funcall f x)
	    (funcall f (+ x dx)))
	 3)))

(defun n-fold-smooth (f n)
  (funcall (repeated #'smooth n) f))

(defun nth-root (x n)
  (fixed-point (funcall (repeated #'average-damp n)
	       #'(lambda (y) (/ x (expt y (- n 1)))))
	       1.0))


(defun repeated-dampen-root (x n times)
  (fixed-point-of-transform
   (lambda (y) (average y (/ x (expt y (- n 1)))))
   (repeated #'average-damp times)
   1.0))


(defun iterative-improve (test improve)
  #'(lambda (fg)
      (labels ((iter (g)
		 (if (funcall test g)
		     g
		     (iter (funcall improve g)))))
	(iter fg))))

(defun sqrt05 (x)
  (funcall (iterative-improve
	    #'(lambda (g)
		(< (abs (- (square g) x)) 0.0001))
	    #'(lambda (g)
		(average g (/ x g))))
	   1.0))
