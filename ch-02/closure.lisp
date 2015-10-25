(defun square (x)(* x x))

(defun length-item (L)
  (if (null L)
      0
      (+ 1 (length-item (rest L)))))

(defun list-ref (items n)
  (if (= n 0)
      (first items)
      (list-ref (rest items) (- n 1))))

(defun length1 (L)
  (labels ((length-iter (l len)
	     (if (null l)
		 len
		 (length-iter (rest l) (incf len)))))
    (length-iter L 0)))

(defun append00 (L1 L2)
  (if (null L1)
      L2
      (cons (car L1) (append (cdr L1) L2))))

(defun last-pair (L)
  (if (null (rest L)) 
      L
      (last-pair (rest L))))

(defun reverse00 (L)
  (if (null L)
      nil
      (append (reverse00 (rest L))(list (car L)))))

;;2-20
(defun same-parity (L)
  (cond ((null L) nil)
	((oddp (car L))(cons (car L) (same-parity (rest L))))
	(t (same-parity (rest L)))))

(defun scale-list (items factor)
  (if (null items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))


(defun mymap (proc items)
  (if (null items)
      nil
      (cons (funcall proc (car items))
	    (mymap proc (cdr items)))))

(defun scale-list1 (items factor)
  (mymap (lambda (x) (* x factor))
	 items))

(defun square-list (L)
  (if (null L)
      nil
      (cons (square (car L)) (square-list (cdr L)))))

(defun square-list (L)
  (mymap (lambda (x) (square x))
	 L))


;;2-22
(defun square-list00 (L)
  (labels ((iter (things answer)
	     (if (null things)
		 answer
		 (iter (cdr things)
		       (append answer
			       (list (square (car things))))))))
			       
    (iter L nil)))

;;2-23
(defun for-each (proc items)
  (if (null items)
      nil
      (progn (funcall proc (car items))
	     (for-each proc (cdr items)))))

(defun for-each00 (proc items)
  (when items
    (funcall proc (car items))
    (for-each00 proc (cdr items))))

(defun count-leaves (x)
  (cond ((null x) 0)
	((not (consp x)) 1)
	(t (+ (count-leaves (car x))
	      (count-leaves (cdr x))))))


;;2-27

(defun deep-reverse (L)
  (if (consp L)
      (append (deep-reverse (cdr L)) (list (deep-reverse (car L))))
      L))

;;2-28
(defun fringe (L)
  (if (consp L)
      (if (consp (car L))
	  (append (fringe (car L)) (fringe (cdr L)))
	  (cons (car x) (fringe (cdr x))))
      L))

;;2-29
(defun make-mobile (left right)
  (list left right))

(defun make-branch (len structure)
  (list len structure))

(defun left-branch (mobile)
  (car mobile))

(defun right-branch (mobile)
  (first (rest mobile)))

(defun branch-length (branch)
  (car branch))

(defun branch-structure (branch)
  (first (rest branch)))



(defun total-weight (mobile)
  (if (atom mobile)
      mobile
      (let ((left-b (left-branch mobile))
	     (right-b (right-branch mobile)))
	(let ((left-s (branch-structure left-b))
	      (right-s (branch-structure right-b)))
	  (+ (total-weight left-s)
	     (total-weight right-s))))))

(defun balanced? (mobile)
    (if (atom mobile)
	t
	(let* ((left-b (left-branch mobile))
	       (right-b (right-branch mobile)))
	  (let* ((left-s (branch-structure left-b))
		 (right-s (branch-structure right-b)))
	    (and
	     (= (* (branch-length left-b)
		   (total-weight left-s))
		(* (branch-length right-b)
		   (total-weight right-s)))
	     (balanced? left-s) (balanced? right-s))))))

(defvar mymobile '((6 ((1 2) (2 1))) (3 ((2 4) (4 2)))))


;;
(defun scale-tree (tree factor)
  (cond ((null tree) nil)
	((atom tree) (* tree factor))
	(t (cons (scale-tree (car tree) factor)
		 (scale-tree (cdr tree) factor)))))

(defun scale-tree-map (tree factor)
  (mymap (lambda (x) 
	   (if (consp x)
	       (scale-tree x factor)
	       (* x factor)))
	 tree))

;;2-30

(defun square-tree (tree)
  (mapcar (lambda (x)
	   (if (consp x)
	       (square-tree x)
	       (square x)))
	 tree))

(defun tree-map (func tree)
  (cond ((null tree) nil)
	((atom tree) (funcall func tree))
	(t (cons (tree-map func (car tree))
		 (tree-map func (cdr tree))))))

(defun square-tree11 (tree)
  (tree-map #'square tree))

(defun sqr (tree)
  (if (consp tree)
      (mapcar (lambda (x) (sqr x)) tree)
      (square tree)))

(defun subset (s)
  (if (null s)
      (list nil)
      (let ((rest (subset (cdr s))))
	(append rest (mapcar (lambda (x) (cons (car s) x)) rest)))))


