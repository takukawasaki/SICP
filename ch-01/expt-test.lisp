d(defun expt-00 (b n)
  (if (= n 0)
      1
      (* b (expt-00 b (- n 1)))))

(defun expt-01 (b n)
  (expt-iter b n 1))

(defun expt-iter (b counter product)
  (if (= counter 0)
      product
      (expt-iter b (- counter 1) (* b product))))

(defun expt-fast (b n)
  (cond ((= n 0) 1)
	((evenp n) (square (expt-fast b (/ n 2))))
	(t (* b (expt-fast b (- n 1))))))
(defun square (x)
  (* x x))

(defun expt-fast2 (b n)
  (expt-iter b n 1))

(defun expt-iter01 (b n a)
  (cond ((= n 0) a)
	((evenp n) (expt-iter01 (square b) (/ n 2) a))
	(t (expt-iter01 b (- n 1) (* b a)))))

(defun pp (a b)
  (cond ((= b 0) 0)
	((evenp b)(doubles (pp a (/ b 2))))
	(t (+ a (pp a (- b 1))))))

(defun doubles (n)
  (+ n n))


(defparameter yy
  (let ((v nil))
    (setf v (mapcar #'(lambda (i)
                        (* i i) )
                    '(1 2 3 4)))
    V))


(defparameter sum nil)

(mapcar (lambda (x)
            (mapcar (lambda (y)
                        (setf sum  (append sum (list x y))))
                      '(3 4)))
          '(1 2))
