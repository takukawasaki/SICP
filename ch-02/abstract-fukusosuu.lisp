(defun add-complex (z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
		       (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
		       (- (imag-part z1) (real-part z2))))

(defun mul-complex (z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
		     (* (angle z1) (angle z2))))

(defun div-complex (z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
		     (- (angle z1) (angle z2))))

(defun real-part-rectangular (z)
  (car z))

(defun imag-part-rectangular (z)
  (cdr z))

(defun magnitude-rectangular (z)
  (sqrt (+ (square (real-part-rectangular z))
	   (square (imag-part-rectangular z)))))

(defun angle-rectangular (z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(defun make-from-real-imag-rectangular (x y)
  (attach-tag 'rectangular (cons x y)))

(defun make-from-mag-ang-rectangular (r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

(defun real-part-polar (z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(defun imag-part-polar (z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(defun magnitude-polar (z)
  (car z))

(defun angle-polar (z)
  (cdr z))

(defun make-from-real-imag-polar  (x y)
  (attach-tag 'polar
	      (cons (sqrt (+ (square x) (square y)))
		    (atan y x))))

(defun make-from-mag-ang-polar (r a)
  (attach-tag 'polar (cons r a)))



(defun attach-tag (type-tag contents)
  (cons type-tag contents))

(defun type-tag (datum)
  (if (consp datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(defun contents (datum)
  (if (consp datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENT-TAG" datum)))

(defun rectangular-p (z)
  (eq (type-tag z) 'rectangular))

(defun polar-p (z)
  (eq (type-tag z) 'polar ))


(defun real-part (z)
  (cond ((rectangular-p z)
	 (real-part-rectangular (contents z)))
	((polar-p z)
	 (real-part-polar (contents z)))
	(t
	 (error "Unknown type -- REAL-PART" z))))

(defun imag-part (z)
  (cond ((rectangular-p z)
	 (imag-part-rectangular (contents z)))
	((polar-p z)
	 (imag-part-polar (contents z)))
	(t
	 (error "Unknown type -- Imag-PART" z))))

(defun magnitude (z)
  (cond ((rectangular-p z)
	 (magnitude-rectangular (contents z)))
	((polar-p z)
	 (magnitude-polar (contents z)))
	(t
	 (error "Unknown type -- MAGNITUDE" z))))

(defun angle (z)
  (cond ((rectangular-p z)
	 (angle-rectangular (contents z)))
	((polar-p z)
	 (angle-polar (contents z)))
	(t
	 (error "Unknown type -- ANGLE" z))))

(defun make-from-real-imag (x y)
  (make-from-real-imag-rectangular x y))
(defun make-from-mag-ang (r a)
  (make-from-mag-ang-polar r a))

(defun install-rectangular-package ()
  (labels ((real-part (z) (car z))
	   (imag-part (z) (cdr z)))))

;;2-73

;;a, numberやvariableにはタグがないので, データ主導にできない.

(defun make-table()
  (let ((local-table (list '*table*)))
    (defun lookup (key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (defun insert! (key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
	(if subtable
	    (let ((record (assoc key-2 (cdr subtable))))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (defun dispatch (m)
      (cond ((eq m 'lookup-proc) lookup)
	    ((eq m 'insert-proc!) insert!)
	    (t (error "Unknown operation -- TABLE" m))))
    dispatch))

(defvar operation-table (make-table))

(defvar get (operation-table 'lookup-proc))

(defvar put (operation-table 'insert-proc!))

(defun deriv (exp var)
  (cond ((numberp exp) 0)
	((variablep exp) (if (same-variable? exp var) 1 0))
	(t (let ((op (get 'deriv (operator exp))))
	     (cond ((op (op (operands exp) var))
		    (t (error "unkown expression -- DERIV" exp))))))))

(defun operator exp) (car exp))

(defun operands (exp) (cdr exp))

(defun deriv-package()
  (defun (deriv-sum exp var)
      (newline)(display (list 'deriv-sum exp var))
      (make-sum (deriv (addend exp) var)
		(deriv (augend exp) var)))
  (defun (deriv-prod exp var)
      (newline)(display (list 'deriv-prod exp var))
      (make-sum
       (make-product (multiplier exp)
		     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
		     (multiplicand exp))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-prod))

(defun variablep (x) (symbolp x))

(defun (same-variablep (v1 v2) (and (variable? v1) (variable? v2)
			       (eq v1 v2))))

(defun addend (s) (car s))  ;operandsが引数なので, dがひとつ少なくなる.
					; cadr → car
(defun augend (s) (cadr s))

(defun multiplier (p) (car p))

(defun multiplicand (p) (cadr p))

(defun make-sum (a1 a2) (list '+ a1 a2))

(defun make-product (m1 m2) (list '* m1 m2))

(deriv-package)

(deriv '(* (* x y) (+ x 3)) 'x)

