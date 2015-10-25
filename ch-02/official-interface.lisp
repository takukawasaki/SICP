(defun square (x)
  (* x x))

(defun prime-p (x)
  (= x (smallest-divisor x)))

(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test)
  (cond ((> (square test) n) n)
	((divides-p n test) test)
	(t (find-divisor n (+ test 1)))))



(defun divides-p (n test)
  (= (rem n test) 0))


(defun sum-odd-squares (tree)
  (cond ((null tree) 0)
	((atom tree)
	 (if (oddp tree) (square tree) p))
	(t (+ (sum-odd-squares (car tree))
	      (sum-odd-squares (cdr tree))))))

(defun fib (n)
  (fib-iter 1 0 n))

(defun fib-iter (a b n)
  (if (= n 0)
      b
      (fib-iter (+ a b) a (- n 1))))


(defun even-fibs (n)
  (labels ((next (k)
	     (if (> k n)
		 nil
		 (let ((f (fib k)))
		   (if (evenp f)
		       (cons f (next (+ k 1)))
		       (next (+ k 1)))))))
    (next 0)))


(defun filter (predicate sequence)
  (cond ((null sequence) nil)
	((funcall predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(t (filter predicate (cdr sequence)))))


;;apply same
(defun accumlation (op initial sequence)
  (if (null sequence)
      initial
      (funcall op (car sequence)
	       (accumlation op initial (cdr sequence)))))

(defun enumerate-tree (tree)
  (cond ((null tree) nil)
	((atom tree) (list tree))
	(t (append (enumerate-tree (car tree))
		   (enumerate-tree (cdr tree))))))

(defun enumerate-interval (low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(defun sum-odd-squares-2 (tree)
  (accumlation #'cons
	       nil
	       (filter #'evenp
		       (mapcar #'fib
			       (enumerate-interval 0 tree)))))

(defun even-fibs-2 (n)
  (accumlation #'cons
	       nil
	       (filter #'evenp
		       (mapcar #'fib
			       (enumerate-interval 0 n)))))


(defun list-fib-square (n)
  (accumlation #'cons
	       nil
	       (mapcar #'square
		       (mapcar #'fib
			       (enumerate-interval 0 n)))))

;;2.33

(defun mappp (p sequence)
  (accumlation (lambda (x y) (cons (funcall p x) y)) nil sequence))

(defun appendd (seq1 seq2)
  (accumlation cons seq2
	       seq1))



(defun length11 (sequence)
  (accumlation (lambda (x y) (+ y 1)) 0 sequence))


(defun leng (L)
  (reduce #'(lambda (x y) (+ x 1)) L :initial-value 0))

(defun horner-eval (x coefficient-sequence)
  (accumlation #'(lambda (this-coeff high-terms)
		   (+ (* high-terms x) this-coeff))
	       0
	       coefficient-sequence))

(defun count-leaves (tree)
  (accumlation + 0 (mapcar #'(lambda (x)
			       (if (consp x)
				   (count-leaves x)
				   1)) tree)))
(defun accumulate-n (op init seqs)
  (if (null (car seqs))
      nil
      (cons (accumlation op init (mapcar #'car seqs))
	    (accumlation-n op init (mapcar #'cdr seqs)))))

(defun dot-product (v w)
  (accumlation + 0 (mapcar #'* v w)))

;;2-37
(defun matrix-*-vector (m v)
  (mapcar #'(lambda (x) (dot-product v x)) m))


(defun transpose (mat)
  (accumlate-n #'cons '() mat))


(defun matrix-*-matrix (m n)
  (let ((cols (transpose n)))
    (mapcar  #'(lambda (x)
		 (mapcar #'(lambda (y) (dot-product x y)) cols)) m)))



;;2-38

(defun fold-left (op initial sequence)
  (labels ((iter (result rest)
	     (if (null rest)
		 result
		 (iter (funcall op result (car rest))
		       (cdr rest)))))
    (iter initial sequence)))

(setf (symbol-function 'fold-right) #'accumlation)


;;2-39
(defun reverse-1 (sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(defun reverse-2 (sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

(defun flat-map (proc seq)
  (accumlation #'append nil (mapcar proc seq)))

(defun prime-sum? (pair)
  (prime-p (+ (car pair) (cadr pair))))

(defun make-pair-sum (pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(defun prime-sum-pairs (n)
  (mapcar #'make-pair-sum
	  (filter #'prime-sum?
		  (flat-map
		   (lambda (i)
		     (mapcar (lambda (j) (list i j))
			     (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))))

(defun permutation (s)
  (if (null s)
      (list nil)
      (mapcan #'(lambda (x)
		  (mapcar #'(lambda (p) (cons x p))
			  (permutation (remove x s :test #'eq))))
	      s)))

(defun unique-pairs (s)
  (flat-map #'(lambda (x)
		(mapcar #'(lambda (y) (list x y))
			(enumerate-interval 1 (- x 1))))
	    (enumerate-interval 1 n)))

(defun prime-sum-pairs-2 (n)
  (mapcar #'make-pair-sum
	  (filter #'prime-sum?
		  (unique-pairs n))))

(defun unique-triple (n)
  (flat-map #'(lambda (x)
		(flat-map #'(lambda (y)
			      (mapcar #'(lambda (z) (list x y z))
				      (enumerate-interval 1 (- y 1))))
			  (enumerate-interval 1 (- x 1))))
	    (enumerate-interval 1 n)))

(defun sum-equal-triple (n s)
  (filter #'(lambda (x) (= (apply #'+ x) s))
	  (unique-triple n)))


(defun queens (board-size)
  (labels ((queen-cols (k)
	     (if (= k 0)
		 (list empty-board)
		 (filter
		  (lambda (positions) (safe? k positions))
		  (flat-map
		   (lambda (rest-of-queens)
		     (mapcar (lambda (new-row)
			       (adjoin-position
				new-row
				k
				rest-of-queens))
			     (enumerate-interval 1 board-size)))
		   (queen-cols (- k 1)))))))
    (queen-cols board-size)))

(defun adjoin-position (new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(defun safe? (k positions)
  (labels ((safe1 (x n)
	     (or (= n k)
		 (let ((y (elt positions n)))
		   (and (not (= x y))
			(not (= (- x y) n))
			(not (= (- y x) n))
			(safe1 x (+ n 1)))))))
    (safe1 (car positions) 1)))

(defvar empty-board nil)


(defun wave2 (beside wave (flip-vert wave)))

(defun wave4 (below wave2 wave2))
