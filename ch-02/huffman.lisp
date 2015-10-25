(require 'data-pointer "~/Dropbox/SICP/BYLISP/ch-02/data-pointer.lisp")

(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf? (object)
  (eq (car object) 'leaf))

(defun symbol-leaf (x)
  (cadr x))

(defun weight-leaf (x)
  (caddr x))

(defun make-code-tree (left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(defun left-branch (tree)
  (car tree))

(defun right-branch (tree)
  (cadr tree))

(defun symbols (tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(defun weight  (tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(defun decode (bits tree)
  (labels ((decode-1 (bits current-branch)
	     (if (null bits)
		 nil
		 (let ((next-branch
			(choose-branch (car bits) current-branch)))
		   (if (leaf? next-branch)
		       (cons (symbol-leaf next-branch)
			     (decode-1 (cdr bits) tree))
		       (decode-1 (cdr bits) next-branch))))))
    (decode-1 bits tree)))

(defun choose-branch (bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(t
	 (error "bad bit == choose-branch" bit))))

(defun adjoin-set-3 (x set)
  (cond ((null set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(t
	 (cons (car set)
	       (adjoin-set-3 x (cdr set))))))

(defun make-leaf-set (pairs)
  (if (null pairs)
      nil
      (let ((pair (car pairs)))
	(adjoin-set-3 (make-leaf (car pair)
				 (cadr pair))
		      (make-leaf-set (cdr pairs))))))

;;2-67
(defparameter *sample-tree*
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))
(defparameter *sample-message* '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;;2-68
(defun encode (message tree)
  (if (null message)
      nil
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(defun encode-symbol (symbol tree)
  (if (leaf? tree) 
      nil
      (let ((lb (left-branch tree))
	    (rb (right-branch tree)))
	(cond ((element-of-set? symbol (symbols lb))
	       (cons 0 (encode-symbol symbol lb)))
	      ((element-of-set? symbol (symbols rb))
	       (cons 1 (encode-symbol symbol rb)))))))


;;2-69
(defun generate-huffman-tree (pair)
  (successive-merge (make-leaf-set pairs)))


(defun successive-merge (set)
  (if (= (length set) 1) (car set)
      (let ((sorted-set (sort set (lambda (x y) (< (weight x) (weight y))))))
	(successive-merge
	 (cons (make-code-tree (car sorted-set) (cadr sorted-set))
	       (cddr sorted-set))))))


;;2-70
((leaf na 16)
 ((leaf yip 9)
  (((leaf a 2) ((leaf wah 1) (leaf boom 1) (wah boom) 2) (a wah boom) 4)
   ((leaf sha 3) ((leaf job 2) (leaf get 2) (job get) 4) (sha job get) 7)
   (a wah boom sha job get)
   11)
  (yip a wah boom sha job get)
  20)
 (na yip a wah boom sha job get)
 36)


;;2-71
(generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16))) ==>

(((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7)
  (leaf d 8)
  (a b c d)
  15)
 (leaf e 16)
 (a b c d e)
 31)


(generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16)
			 (f 32) (g 64) (h 128) (i 256) (j 512))) ==>

((((((((((leaf a 1) (leaf b 2) (a b) 3) (leaf c 4) (a b c) 7)
       (leaf d 8)
       (a b c d)
       15)
      (leaf e 16)
      (a b c d e)
      31)
     (leaf f 32)
     (a b c d e f)
     63)
    (leaf g 64)
    (a b c d e f g)
    127)
   (leaf h 128)
   (a b c d e f g h)
   255)
  (leaf i 256)
  (a b c d e f g h i)
  511)
 (leaf j 512)
 (a b c d e f g h i j)
 1023)

;;最高頻度 1ビット
;;最低頻度 n-1ビット

;;2-72


;;最高頻度の記号は
;;encode-symbol     2回
;;element-of-set? n+1回

;;最低頻度の記号は
;;encode-symbol     n回
;;element-of-set? n-1回
