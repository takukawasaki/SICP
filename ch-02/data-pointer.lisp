;;2-53
;;(a b c)
;;((geoge))
;;

(defun equal? (a b)
  (cond ((and (atom a) (atom b))
	 (eq a b))
	((and (consp a) (consp b))
	 (and (equal? (car a) (car b))
	      (equal? (cdr a) (cdr b))))
	(t nil)))

(car ''(abracadabra))

;;2-3-2
;;deriv

(defun deriv (exp var)
  (cond ((numberp exp) 0)
	((variable? exp)
	 (if (same-variable? exp var) 1 0))
	((sum? exp)
	 (make-sum (deriv (addend exp) var)
		   (deriv (augend exp) var)))
	((product? exp)
	 (make-sum (make-product (multiplier exp)
				 (deriv (multiplicand exp) var))
		   (make-product (deriv (multiplier exp) var)
				 (multiplicand exp))))
	((exponation? exp)
	 (make-product (exponent  exp)
		       (make-product
			(make-exponent
			 (base exp) (- (exponent exp) 1))
			(deriv (base exp) var))))
	(t (error "unknown expression type ---Deriv" exp))))

(defun variable? (x)
  (symbolp x))

(defun same-variable? (val1 val2)
  (and (variable? val1) (variable? val2) (eq val1 val2)))

(defun =number? (exp num)
  (and (numberp exp) (= exp num)))


(defun make-sum (a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (numberp a1) (numberp a2))
	 (+ a1 a2))
	(t (list '+ a1 a2))))

(defun make-product (m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (numberp m1) (numberp m2))
	 (* m1 m2))
	(t (list '* m1 m2))))

(defun sum? (x)
  (and (consp x) (eq (car x) '+)))

(defun addend (s) (cadr s))
(defun augend (s)
  (if (null (cdddr s))
      (caddr s)
      (cons '+ (cddr s))))

(defun product? (x)
  (and (consp x) (eq (car x) '*)))

(defun multiplier (p) (cadr p))
(defun multiplicand (p)
  (if (null (cdddr p))
      (caddr p)
      (cons '* (cddr p))))


;;2-56
(defun pow (a b)
  (expt a b))

(defun exponation? (x)
  (and (consp x) (eq (car x) '**)))

(defun base (x)
  (cadr x))

(defun exponent (x)
  (caddr x))


(defun make-exponent (base exp)
  (cond ((=number? exp 0) 1)
	((=number? exp 1) base)
	(t (list '** base exp))))



;;2-57

(defun make-sum-middle (a1 a2)
  (list a1 '+ a2))

(defun make-product-middle (a1 a2)
  (list a1 '* a2))

(defun sum-middle? (exp)
  (and (consp exp) (eq (cadr exp) '+)))

(defun product-middle? (exp)
  (and (consp exp) (eq (cadr exp) '*)))

(defun addend-middle (exp)
  (car exp))

(defun augent-middle (exp)
  (caddr exp))


(defun multiplier-middle (exp)
  (car exp))


(defun multiplicand-middle (exp)
  (caddr exp))


(defun compiles (exp)
  (cond ((symbolp exp) exp)
	((numberp exp) exp)
	((eq (car exp) '+) exp)
	((eq (car exp) '*) exp)
	((= (length exp) 3)
	 (list (cadr exp) (compiles (car exp)) (compiles (caddr exp))))
	((>= (length exp) 4)
	 (cond ((eq (cadr exp) (cadddr exp))
		(compiles (cons (list (cadr exp) (compiles (car exp))
				      (compiles (caddr exp)))
				(cdddr exp))))
	       ((and (eq (cadr exp) '+) (eq (cadddr exp) '*))
		(let ((a (car exp))
		      (b (cadr exp))
		      (c (caddr exp))
		      (d (cadddr exp))
		      (e (list-ref exp 4))
		      (f (list-tail exp 5)))
		  (compiles (cons a (cons b (cons
					     (list d
						   (compiles c)
						   (compiles e)) f))))))
	       ((and (eq (cadr exp) '*) (eq (cadddr exp) '+))
		(let ((a (car exp))
		      (b (cadr exp))
		      (c (caddr exp))
		      (d (cadddr exp))
		      (e (list-ref exp 4))
		      (f (list-tail exp 5)))
		  (compiles (cons d
				  (cons (list (compiles b)
					      (compiles a)
					      (compiles c) )
					(cons (compiles e) f))))))))))

;;2-3-3
(defun element-of-set? (x set)
  (cond ((null set) nil)
	((equal? x (car set)) t)
	(t (element-of-set? x (cdr set)))))

(defun adjoin-set (x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(defun intersection-set (set1 set2)
  (cond ((or (null set1) (null set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1) set2)))
	(t (intersection-set (cdr set1) set2))))

;;2-59

(defun union-set (set1 set2)
  (cond ((null set1) set2)
	((element-of-set? (car set1) set2)
	 (union-set (cdr set1) set2))
	(t
	 (cons (car set1)
	       (union-set (cdr set1) set2)))))


;;2-60
(defun adjoin-set2 (x set)
  (cons x set))

(defun union-set2 (set1 set2)
  (cons set1 set2))

(defun element-of-set?-sort (x set)
  (cond ((null set) nil)
	((= x (car set) t))
	((< x (car set) nil))
	(t (element-of-set? x (cdr set)))))


(defun intersection-set-sort (set1 set2)
  (if (or (null set1) (null set2))
      nil
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set-sort (cdr set1)
					       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set-sort (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set-sort set1 (cdr set2)))))))


;;2-61
(defun adjoin-set-sort (x set2)
  (cond ((null set) (list x))
	((= x (car set2) set2))
	((< x (car set2) (cons x set2)))
	(t
	 (cons (car set2) (adjoin-set-sort x (cdr set2))))))

;;2-62

(defun union-set-sort (set1 set2)
  (cond ((null set1) set2)
	((null set2) set1)
	(t
	 (let ((x1 (car set1))
	       (x2 (car set2)))
	   (cond ((= x1 x2)
		  (cons x1 (union-set-sort (cdr set1) (cdr set2))))
		 ((< x1 x2)
		  (cons x1 (union-set-sort (cdr set1) set2)))
		 ((> x1 x2)
		  (cons x2 (union-set-sort set1 (cdr set2)))))))))

;;binary tree

(defun entry (tree)
  (car tree))

(defun left-branch (tree)
  (cadr (tree)))

(defun right-branch (tree)
  (caddr tree))

(defun make-tree (entry left right)
  (list entry left right))

(defun element-of-set-2? (x set)
  (cond ((null set) nil)
	((= x (entry set)) t)
	((< x (entry set))
	 (element-of-set-2? x (left-branch set)))
	((> x (entry set))
	 (element-of-set-2? x (right-branch set)))))

(defun adjoin-set-2 (x set)
  (cond ((null set) (make-tree (x () ())))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set-2 x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set-2 x (right-branch set))))))

;;2-64



;;a. 二つの手続きとも, 同じリストを生成する.

;;partial-treeの働き:

;;eltsはソートしてあると仮定．
;;n = 0 の時 (() . elts) を返す．
;;n > 0 の時 (eltsの最左n個の釣り合った二進木 . eltsの残り) を返す．
;;n個のうち左部分木になる個数 left-size (n - 1)/2 (1はthis-entryのための数
;;でpartial-treeを呼ぶ
;;(左部分木 ．残り) が返る．左部分木をleft-treeに記憶．残りをnon-left-tree
;;とする．non-left-treeの先頭をthis-entryとして記憶．その残りからright-size
;;個の右部分木を作りに行く
;;(右部分木 ．残り) が返る．右部分木をright-treeに記憶．残りを remaining-elts
;;とする．
;;make-treeを使い，((this-entry left-tree right-tree) . remaining-elts)
;;を返す．

;;(1 3 5 7 9 11)をpartial-treeにかける．長さは6だから，left-sizeは2．
;;次のleft-sizeは0．(() . (1 3 5 7 9 11)) が返る．right-sizeは1で
;;(1 () (3 () ())) が左部分木．5がthis-entryになり，(7 9 11)のpartial-tree
;;の結果は (9 (7 () ()) (11 () ()))
;;そこで全体は
;;(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))))

;;2-65

(defun union-set-tree (tree1 tree2)
  (list->tree (union-set (tree->list-1 tree1)
			 (tree->list-1 tree2))))

(defun intersection-set-tree (tree1 tree2)
  (list->tree (intersection-set (tree->list-1 tree1)
				(tree->list-1 tree2))))

;;data-base

(defun lookup (key record)
  (cond ((null record) nil)
	((equal? key (key (car record)))
	 (car record))
	(t
	 (lookup key (cdr record)))))

;;2-66

(defun look-up (given-key set-of-record)
  (cond ((null set-of-record) nil)
	((equal? given-key (key (car set-of-record)))
	 (car set-of-record))
	((< (order given-key) (order (key (car set-of-record))))
	 (look-up given-key (cadr set-of-record)))
	(else (look-up given-key (caddr set-of-record)))))
;;orderはkeyを数値に変換
