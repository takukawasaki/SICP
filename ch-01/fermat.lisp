(load "~/Dropbox/SICP/BYLISP/ch-01/small-divisor.lisp")

(defun expmod (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (mod (square (expmod base (/ exp 2) m)) m))
	(t (mod (* base (expmod base (- exp 1) m)) m))))

(defun fermat-test (n)
  (labels ((try-it (a)
	     (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(defun fermat (n)
  (let ((try (lambda (a) (= (expmod a n n) a))))
    (funcall try (+ 1 (random (- n 1))))))


(defun fast-prime (n times)
  (cond ((= times 0) t)
	((millar-labin-test n) (fast-prime n (- times 1)))
	(t nil)))

(defun search-prime (n)
  (search-for-prime n 0))

(defun search-for-prime (n times)
  (cond ((= times 3) t)
	((fermat-test n) (princ n)(fresh-line) (search-for-prime (+ n 1) (+ times 1)))
	(t (search-for-prime (+ n 1) times))))



(defun expmod2 (base exp m)
  (cond ((= exp 0) 1)
	((evenp exp)
	 (let* ((x (expmod2 base (/ exp 2) m))
		(y (mod (square x) m)))
	   (if (and (/= x 1) (/= x (- m 1)) (= y 1))
	       0
	       y)))
	(t (mod (* base (expmod2 base (- exp 1) m)) m))))

(defun millar-labin-test (n)
  (if (< n 2)
      nil
      (let* ((a (+ 1 (random (- n 1)))))
	(= (expmod2 a n n) a))))
