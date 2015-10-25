(defun add (x y)
  (apply-generic 'add x y))

(defun sub (x y)
  (apply-generic 'sub x y))

(defun mul (x y)
  (apply-generic 'mul x y))

(defun div (x y)
  (apply-generic 'div x y))

(defun install-scheme-number-package ()
  (labels ((tag (x)
             (attach-tag 'scheme-number x)))
    (put 'add '(scheme-number scheme-number)
         (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
         (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
         (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
         (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
         (lambda (x) (tag x)))
    'done))


(defun make-scheme-number (n)
  ((get 'make 'scheme-number) n))


(defun install-rational-package ()
  (labels ((numer (x) (car x))
           (denom (x) (cdr x))
           (make-rat (n d)
             (let ((g (gcd n d)))
               (cons (/ n g) (/ d g))))
           (add-rat (x y)
             (make-rat (+ (* (numer x) (denom y))
                          (* (numer y) (denom x)))
                       (* (denom x) (denom y))))
           (sub-rat (x y)
             (make-rat (- (* (numer x) (denom y))
                          (* (numer y) (denom x)))
                       (* (denom x) (denom y))))
           (mul-rat (x y)
             (make-rat (* (numer x) (numer y))
                       (* (denom x) (denom y))))
           (div-rat (x y)
             (make-rat (* (numer x) (denom y))
                       (* (denom x) (numer y))))
           (tag (x) (attach-tag 'rational x)))
    (put 'add '(rational rational)
         (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
         (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
         (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
         (lambda (x y) (tag (div-rat x y))))
    (put 'make 'rational
         (lambda (n d) (tag (make-rat n d))))
    'done))

(defun make-rational (n d)
  ((get 'make 'rational ) n d))



(defun install-complex-package ()
  (labels ((make-from-real-imag (x y)
             ((get 'make-from-real-imag 'rectangular) x y))
           (make-from-mag-ang (r a)
             ((get 'make-from-mag-ang 'polat) r a))
           (add-complex (z1 z2)
             (make-from-real-imag (+ (real-part z1) (real-part z2))
                                  (+ (imag-part z1) (imag-part z2))))
           (sub-complex (z1 z2)
             (make-from-real-imag (- (real-part z1) (real-part z2))
                                  (- (imag-part z1) (imag-part z2))))
           (mul-complex (z1 z2)
             (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                                (+ (angle z1) (angle z2))))
           (div-complex (z1 z2)
             (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                                (- (angle z1) (angle z2))))
           (tag (z) (attach-tag 'complex z)))
    (put 'add '(complex complex)
         (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
         (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
         (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
         (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-mag-ang 'complex
         (lambda (r a) (tag (make-from-mag-ang r a))))
    'done))

(defun make-complex-from-real-imag (x y)
  ((get 'make-from-real-imag 'complex) r a))

(defun make-complex-from-mag-ang (r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; 2.78

(defun type-tag (datum)
  (cond ((numberp datum) 'scheme-number)
        ((consp datum) (car datum))
        (t (error "Bad tagged datum -- TYPE-TAG" datum))))

(defun contents (datum)
  (cond ((numberp datum) datum)
        ((consp datum) (cdr datum))
        (t (error "Bad tagged datum -- CONTENTS" datum))))

(defun attach-tag (tag-type contents)
  (if (eq tag-type 'scheme-number)
      contents
      (cons tag-type contents)))



;;2-79


(defun equ? (x y) (apply-generic 'equ? x y))

;;2-80

(defun =zero? (x) (apply-generic '=zero? x))

(defun apply-generic (op &rest args)
  (let ((type-tags (mapcat type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (mapcar contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a1 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (t
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


;;2-81


;;a. complexに型変換してまた手続きを探しに行くから, 無限ループに陥る. 

;;2-82

(defun apply-generic (op &rest args)
  (labels  ((try-coercion (types)
              (labels ((convert (type args)
                         (if (null? type) '()
                             (cons ((get-coercion (car type) (car types)) (car args))
                                   (convert (cdr type) (cdr args)))))
                       (if (null? types)
                           (error "No method for these types"
                                  (list op type-tags))
                           (eval (cons apply-generic (cons op (convert type-tags args)))))))))
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (try-coercion type-tags))))))


;;2-83


(defun raise (x) (apply-generic 'raise x))

(defun make-integer (x) (attach-tag 'integer x))

(defun make-real (x) (attach-tag 'real x))

(defun make-rational (n d)
  (labels ((make-rat n d)
           (let ((g (gcd n d)))
             (cons (/ n g) (/ d g))))
    (attach-tag 'rational (make-rat n d))))

(defun make-complex (r i) (attach-tag 'complex (cons r i)))

(put 'raise '(integer)
     (lambda (x) (make-rational x 1)))

(put 'raise '(rational)
     (lambda (x) (make-real (/ (car x) (cdr x)))))

(put 'raise '(real)
     (lambda (x) (make-complex x 0)))


(defvar a (make-integer 5))

(defvar b (raise a))

(defvar c (raise b))

(defvar d (raise c))

;;2-85


(defun project (x) (apply-generic 'project x))

(put 'project '(complex)
     (lambda (x) (make-real (car x))))

(put 'project '(real)
     (lambda (x) (make-rational (inexact->exact (truncate  x)) 1)))

(put 'project '(rational)
     (lambda (x) (make-integer (inexact->exact (truncate (/ (car x) (cdr x)))))))

(defvar a (make-complex 2.71 3.14))

(defvar b (project a))

(defvar c (project b))

(defvar d (project c))

(defun =? (x y) (apply-generic '=? x y))

                                        ;=? も定義する.

(put '=? '(complex complex)
     (lambda (x y) (and (= (car x) (car y)) (= (cdr x) (cdr y)))))

(put '=? '(real real)
     (lambda (x y) (= x y)))

(put '=? '(rational rational)
     (lambda (x y) (and (= (car x) (car y)) (= (cdr x) (cdr y)))))

(put '=? '(integer integer)
     (lambda (x y) (= x y)))

(defun drop (x)
  (if (eq (car x) 'integer) x
      (let ((y (project x)))
        (if (=? x (raise y)) (drop y) x))))





;;2-87

(defun zero?-poly (p)
  (zero?-terms (term-list p)))

(defun zero?-terms (L)
  (or (empty-termlist? L)
      (and (=zero? (first-term L)) (zero?-terms (rest-terms L)))))

(put '=zero? '(polynomial)
     (lambda (p) (zero?-poly p)))


;;2-88

(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))
(put 'negate '(polynomial)
     (lambda (p1) (tag (negate-poly p1))))

(defun sub-poly (p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (sub-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

(defun sub-terms (L1 L2)
  (add-terms L1
             (map (lambda (t0) (make-term (order t0) (negate (coeff t0)))) L2)))


;;2-89



(defun dense->sparse (terms)
  (labels ((term order (terms)
                 (if (< order 0) '()
                     (cons (list order (car terms)) (term (- order 1) (cdr terms))))))
    (term (- (length terms) 1) terms)))

;;2-91

(defun div-terms (L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms
                      (sub-terms
                       L1
                       (mul-term-by-all-terms
                        (make-term new-o new-c)
                        L2))
                      L2)
                      ))
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))
;;2-93

(defvar p1 (make-polynomial 'x '((2 1) (0 1))))
(defvar p2 (make-polynomial 'x '((3 1) (0 1))))
(defvar rf (make-rational p2 p1))

;;2-94

(defun gcd-poly (p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (let ((terms (gcd-terms (term-list p1) (term-list p2))))
                   (let ((coeff (map coeff terms)))
                     (let ((gcd (gcd-list (cdr coeff) (car coeff))))
                       (car (div-terms terms (list (make-term 0 gcd))))))))
      (error "Polys not in same var -- GCD-POLY"
             (list p1 p2))))

(defun gcd-terms (a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(defun  remainder-terms (a b)
  (cadr (div-terms a b)))

(put 'greatest-common-divisor '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))

(defvar p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(defvar p2 (make-polynomial 'x '((3 1) (1 -1))))

(greatest-common-divisor p1 p2)  ==> (polynomial x (2 -1) (1 1))


;;2-96

(defun pseudoremainder-terms (p q)
  (let ((fp (first-term p)) (fq (first-term q)))
    (let ((o1 (order fp)) (o2 (order fq)) (c (coeff fq)))
      (let ((int-fact (expt c (+ 1 o1 (- o2)))))
        (cadr (div-terms
               (mul-term-by-all-terms (make-term 0 int-fact) p) q))))))

;;2-97


(defun reduce-poly (p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (let ((res (reduce-terms (term-list p1) (term-list p2))))
        (list (make-poly (variable p1) (car res))
              (make-poly (variable p2) (cadr res))))
      (error "Polys not in same var -- REDUCE-POLY"
             (list p1 p2))))

(defun reduce-terms (term1 term2)
  (let ((gcd (gcd-terms (term-list term1) (term-list term2))))
    (princ (list 'gcd gcd)) (fresh-line)
    (let ((o1 (max (order (first-term term1))
                   (order (first-term term2))))
          (o2 (order (first-term gcd)))
          (c (coeff (first-term gcd))))
      (princ (list 'o1 o1 'o2 o2 'c c)) (fresh-line)
      (let ((int-fact (expt c (+ 1 o1 (- o2)))))
        (let ((nn (car
                   (div-terms (mul-term-by-all-terms (make-term 0 int-fact)
                                                     term1) gcd)))
              (dd (car
                   (div-terms (mul-term-by-all-terms (make-term 0 int-fact)
                                                     term2) gcd))))
          (let ((coeff (append (mapcar coeff term1) (mapcar coeff term2))))
            (let ((d (gcd-list (cdr coeff) (car coeff))))
              (princ (list nn dd d)) (fresh-line)
              (list (car (div-terms nn (list (make-term 0 d))))
                    (car  (div-terms dd (list (make-term 0 d))))))))))))


;;original-scheme-code


;;算術演算

(define (square x) (* x x))

(define (apply-generic op . args)                ;; p.107
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (error
             "No method for these types -- APPLY-GENERIC"
             (list op type-tags))))))

(define (attach-tag type-tag contents)          ;;p.102
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)))

(define (make-table)                                       ;;p.159
    (let ((local-table (list '*table*)))
      (define (lookup key-1 key-2)
          (let ((subtable (assoc key-1 (cdr local-table))))
            (if subtable
                (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (cdr record)
                      false))
                false)))
      (define (insert! key-1 key-2 value)
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
      (define (dispatch m)
          (cond ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation -- TABLE" m))))
      dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (add x y) (apply-generic 'add x y))         ;;p.110

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (real-part z) (apply-generic 'real-part z)) ;;p.107

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (install-scheme-number-package)             ;;p.111
    (define (tag x)
        (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)

(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

(define (install-rational-package)                    ;;p.111

    (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
  (define (add-rat x y)
      (make-rat (+ (* (numer x) (denom y))
                   (* (numer y) (denom x)))
                (* (denom x) (denom y))))
  (define (sub-rat x y)
      (make-rat (- (* (numer x) (denom y))
                   (* (numer y) (denom x)))
                (* (denom x) (denom y))))
  (define (mul-rat x y)
      (make-rat (* (numer x) (numer y))
                (* (denom x) (denom y))))
  (define (div-rat x y)
      (make-rat (* (numer x) (denom y))
                (* (denom x) (numer y))))

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
    ((get 'make 'rational) n d))

(define (install-rectangular-package)          ;;p.106

    (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
      (sqrt (+ (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
      (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (install-polar-package)                 ;;p.107

    (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
      (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
      (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

(define (install-complex-package)                    ;;p.112

    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
      ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                           (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))

  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (cons 'rectangular (cons x y))))
  (put 'make-from-man-ang 'polar
       (lambda (x y) (cons 'polar (cons x y))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))

多項式算術演算

;; (sumsq a b c)のテスト用

;;; genarith2.scmは negate, coercion, poly演算を組み込む. 除算も完成している

;;; genarith1.scmはequ? =zero?が組み込んである.

(define (square x) (* x x))

                                        ;(define (apply-generic op . args)              ;;p.107
                                        ;(display (list 'apply-generic op args)) (newline)
                                        ;  (let ((type-tags (map type-tag args)))
                                        ;    (let ((proc (get op type-tags)))
                                        ;      (if proc
                                        ;          (apply proc (map contents args))
                                        ;          (error
                                        ;            "No method for these types -- APPLY-GENERIC"
                                        ;            (list op type-tags))))))

(define (attach-tag type-tag contents)          ;;p.102
    (if (eq? type-tag 'scheme-number)              ;; ex2.98
        contents
        (cons type-tag contents)))

(define (type-tag datum)
    (cond ((pair? datum) (car datum))             ;; ex2.98
          ((number? datum) 'scheme-number)
          (else  (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
    (cond ((pair? datum) (cdr datum))             ;; ex2.98
          ((number? datum) datum)
          (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (make-table)                                       ;;p.159
    (let ((local-table (list '*table*)))
      (define (lookup key-1 key-2)
          (let ((subtable (assoc key-1 (cdr local-table))))
            (if subtable
                (let ((record (assoc key-2 (cdr subtable))))
                  (if record
                      (cdr record)
                      false))
                false)))
      (define (insert! key-1 key-2 value)
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
      (define (dispatch m)
          (cond ((eq? m 'lookup-proc) lookup)
                ((eq? m 'insert-proc!) insert!)
                (else (error "Unknown operation -- TABLE" m))))
      dispatch))

(define operation-table (make-table))

(define get (operation-table 'lookup-proc))

(define put (operation-table 'insert-proc!))

(define (add x y) (apply-generic 'add x y))         ;;p.110

(define (sub x y) (apply-generic 'sub x y))

(define (mul x y) (apply-generic 'mul x y))

(define (div x y) (apply-generic 'div x y))

(define (remainder x y) (apply-generic 'remainder x y))

(define (reduce x y) (apply-generic 'reduce x y))     ;;ex2.97

(define (real-part z) (apply-generic 'real-part z)) ;;p.107

(define (imag-part z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (equ? x y) (apply-generic 'equ? x y))        ;; equ?

(define (=zero? x) (apply-generic '=zero? x))      ;; =zero?

(define (negate x) (apply-generic 'negate x))      ;; negate

(define (greatest-common-divisor x y)
    (apply-generic 'greatest-common-divisor x y))       ;; gcd

(define (sumsq a b c) (apply-generic 'sumsq a b c))        ;; sumsq test

(define (install-scheme-number-package)             ;;p.111
    (define (tag x)
        (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'euq? '(scheme-number scheme-number)          ;; equ?
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)                      ;; =zero?
       (lambda (x) (= x 0)))
  (put 'negate '(scheme-number)                    ;; negate
       (lambda (x) (tag (- x))))
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (a b) (gcd a b)))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (a b) (let ((g (greatest-common-divisor a b)))
                       (list (a / g) (b / g)))))                  ;; reduce

  (put 'sumsq '(scheme-number scheme-number scheme-number)
       (lambda (a b c) (tag (+ (* a a) (* b b) (* c c)))))      ;; sumsq test

  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(install-scheme-number-package)



(define (make-scheme-number n)
    ((get 'make 'scheme-number) n))

(define (install-rational-package)                    ;;p.111

    (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      ;;;(display (list 'make-rat n d)) (newline)

      (let ((g (greatest-common-divisor n d)))
        (cons (div n g) (div d g))))
  (define (add-rat x y)                               ;;加減乗除は汎用演算に変更してある.
      (make-rat (add (mul (numer x) (denom y))
                     (mul (numer y) (denom x)))
                (mul (denom x) (denom y))))
  (define (sub-rat x y)
      (make-rat (- (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
                (mul (denom x) (denom y))))
  (define (mul-rat x y)
      (make-rat (mul (numer x) (numer y))
                (mul (denom x) (denom y))))
  (define (div-rat x y)
      (make-rat (mul (numer x) (denom y))
                (mul (denom x) (numer y))))

  (define (equ?-rat x y)
      (or (and (equ? (numer x) (numer y)) (equ? (denom x) (denom y)))
          (and (equ? (numer x) (negagte (numer y)))
               (equ? (denom x) (negate (denom y)))))) ;; equ?

  (define (=zero?-rat x) (= (numer x) 0))                    ;; =zero?

  (define (negate-rat x) (make-rat (- (numer x)) (denom x)))   ;; negate

  (define (sumsq-rat a b c)
      (let ((an (square (numer a))) (ad (square (denom a)))
            (bn (square (numer b))) (bd (square (denom b)))
            (cn (square (numer c))) (cd (square (denom c))))
        (make-rat
         (add (mul an (mul bd cd)) (add (mul bn (mul cd ad))
                                        (mul cn (mul ad bd))))
         (mul ad (mul bd cd)))))                    ;; sumsq test

  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'equ? '(rational rational)
       (lambda (x y) (equ?-rat x y)))                   ;; equ?
  (put '=zero? '(rational)                            ;;=zero?
       (lambda (x) (=zero?-rat x)))
  (put 'negate '(rational)
       (lambda (x) (tag (negate-rat x))))                  ;; negate

  (put 'sumsq '(rational rational rational)
       (lambda (a b c) (tag (sumsq-rat a b c))))     ;; sumsq test
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
    ((get 'make 'rational) n d))

(define (install-rectangular-package)          ;;p.106

    (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
      (sqrt (+ (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
      (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
      (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-rectangular-package)

(define (install-polar-package)                 ;;p.107

    (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
      (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
      (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
      (cons (sqrt (+ (square x) (square y)))
            (atan y x)))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)

(define (install-complex-package)                    ;;p.112

    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
      ((get 'make-from-mag-ang 'polar) r a))

  (define (add-complex z1 z2)
      (make-from-real-imag (+ (real-part z1) (real-part z2))
                           (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
      (make-from-real-imag (- (real-part z1) (real-part z2))
                           (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
      (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                         (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
      (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                         (- (angle z1) (angle z2))))

  (define (equ?-complex z1 z2)                           ;; equ?
      (and (= (real-part z1) (real-part z2))
           (= (imag-part z1) (imag-part z2))))

  (define (=zero?-complex z)
      (and (= (real-part z) 0) (= (imag-part z) 0)))       ;; =zero?

  (define (negate-complex z)
      (make-from-real-imag (- (real-part z)) (- (imag-part z))))  ;; negate

  (define (sumsq-complex a b c)
      (let ((ar (real-part a)) (ai (imag-part a)) (br (real-part b))
            (bi (imag-part b)) (cr (real-part c)) (ci (imag-part c)))
        (make-from-real-imag
         (add (sub (mul ar ar) (mul ai ai))
              (add (sub (mul br br) (mul bi bi))
                   (sub (mul cr cr) (mul ci ci))))
         (add (add (mul ar ai) (mul ai ar))
              (add (add (mul br bi) (mul bi br))
                   (add (mul cr ci) (mul ci cr)))))))   ;;sumsq test
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))

  (put 'equ? '(complex complex)
       (lambda (z1 z2) (equ?-complex z1 z2)))            ;; equ?
  (put '=zero? '(complex)                                 ;; =zero?
       (lambda (z) (=zero?-complex z)))
  (put 'negate '(complex)
       (lambda (z) (tag (negate-complex z))))                  ;; negate
  (put 'sumsq '(complex complex complex)
       (lambda (a b c) (tag (sumsq-complex a b c))))       ;; sumsq test

  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)                  ;; ex2.77
  (put 'imag-part '(complex) imag-part)                  ;; ex2.77
  (put 'magnitude '(complex) magnitude)                  ;; ex2.77
  (put 'angle '(complex) angle)                          ;; ex2.77
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (cons 'rectangular (cons x y))))
  (put 'make-from-man-ang 'polar
       (lambda (x y) (cons 'polar (cons x y))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
    ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
    ((get 'make-from-mag-ang 'complex) r a))

                                        ;(define r0 (make-rational 1 2))
                                        ;(define r1 (make-rational 1 3))
                                        ;(add r0 r1)
                                        ;
                                        ;
                                        ;
                                        ;(define z0 (make-complex-from-real-imag 3 4))
                                        ;
                                        ;(define z1 (make-complex-from-real-imag 5 6))
                                        ;
                                        ;(define z2 (add z0 z1))


                                        ;(magnitude z0)

                                        ;(display (equ? z0 z0))

                                        ;(display (equ? z0 z1))

(define coercion-table (make-table))

(define get-coercion (coercion-table 'lookup-proc))

(define put-coercion (coercion-table 'insert-proc!))

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (= (length args) 2)
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                           (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                           (apply-generic op a1 (t2->t1 a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))))
                (error "No method for these types"
                       (list op type-tags)))))))

(define (scheme-number->rational n)
    (make-rational (contents n) 1))
(define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'rational scheme-number->rational)
(put-coercion 'scheme-number 'complex scheme-number->complex)

                                        ;(define z (make-complex-from-real-imag 3 4))
                                        ;(define a (make-scheme-number 5))
                                        ;(add z a)

;; negate test
(define z (make-complex-from-real-imag 3 4))
(negate z)
(negate (make-scheme-number 12))


(define (install-polynomial-package)

    (define (make-poly variable term-list)
        (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
      (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x)) ;;2.3.2節の手続き same-variable? と variable?

  (define (adjoin-term term term-list)
      (if (=zero? (coeff term))
          term-list
          (cons term term-list)))

  (define (the-empty-termlist) '())

  (define (first-term term-list) (car term-list))

  (define (rest-terms term-list) (cdr term-list))

  (define (empty-termlist? term-list) (null? term-list))

  (define (make-term order coeff) (list order coeff))

  (define (order term) (car term))

  (define (coeff term) (cadr term))

  (define (add-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (add-terms (term-list p1)
                                (term-list p2)))
          (error "Polys not in same var -- ADD-POLY"
                 (list p1 p2))))

  (define (add-terms L1 L2)
      (cond ((empty-termlist? L1) L2)
            ((empty-termlist? L2) L1)
            (else
             (let ((t1 (first-term L1)) (t2 (first-term L2)))
               (cond ((> (order t1) (order t2))
                      (adjoin-term
                       t1 (add-terms (rest-terms L1) L2)))
                     ((< (order t1) (order t2))
                      (adjoin-term
                       t2 (add-terms L1 (rest-terms L2))))
                     (else
                      (adjoin-term
                       (make-term (order t1)
                                  (add (coeff t1) (coeff t2)))
                       (add-terms (rest-terms L1)
                                  (rest-terms L2)))))))))

  (define (sub-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (sub-terms (term-list p1)
                                (term-list p2)))
          (error "Polys not in same var -- SUB-POLY"
                 (list p1 p2))))

  (define (sub-terms L1 L2)
      (add-terms L1
                 (map (lambda (t0) (make-term (order t0) (negate (coeff t0)))) L2)))

  (define (mul-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (mul-terms (term-list p1)
                                (term-list p2)))
          (error "Polys not in same var -- MUL-POLY"
                 (list p1 p2))))

  (define (mul-terms L1 L2)
      (if (empty-termlist? L1)
          (the-empty-termlist)
          (add-terms (mul-term-by-all-terms (first-term L1) L2)
                     (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
          (the-empty-termlist)
          (let ((t2 (first-term L)))
            (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (div-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (car (div-terms (term-list p1)
                                     (term-list p2))))
          (error "Polys not in same var -- DIV-POLY"
                 (list p1 p2))))

  (define (remainder-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (cadr (div-terms (term-list p1)
                                      (term-list p2))))
          (error "Polys not in same var -- REMAINDER-POLY"
                 (list p1 p2))))

  (define (div-terms L1 L2)
      (if (empty-termlist? L1)
          (list (the-empty-termlist) (the-empty-termlist))
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (if (> (order t2) (order t1))
                (list (the-empty-termlist) L1)
                (let ((new-c (div (coeff t1) (coeff t2)))
                      (new-o (- (order t1) (order t2))))
                  (let ((rest-of-result
                         (div-terms
                          (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2)
                          ))
                    (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                          (cadr rest-of-result))
                    ))))))

  (define (pseudoremainder-terms p q)
      (let ((fp (first-term p)) (fq (first-term q)))
        (let ((o1 (order fp)) (o2 (order fq)) (c (coeff fq)))
          (let ((int-fact (expt c (+ 1 o1 (- o2)))))
            (cadr (div-terms (mul-term-by-all-terms (make-term 0 int-fact) p) q))))))

  (define (gcd-list l d)
      (if (null? l) d (gcd-list (cdr l) (gcd d (car l)))))

  (define (gcd-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (make-poly (variable p1)
                     (let ((terms (gcd-terms (term-list p1) (term-list p2))))
                       (let ((coeff (map coeff terms)))
                         (let ((gcd (gcd-list (cdr coeff) (car coeff))))
                           (car (div-terms terms (list (make-term 0 gcd))))))))
          (error "Polys not in same var -- GCD-POLY"
                 (list p1 p2))))

  (define (gcd-terms a b)
      (if (empty-termlist? b)
          a
          (gcd-terms b (pseudoremainder-terms a b))))

  (define (remainder-terms a b)
      (cadr (div-terms a b)))

  (define (reduce-poly p1 p2)
      (if (same-variable? (variable p1) (variable p2))
          (let ((res (reduce-terms (term-list p1) (term-list p2))))
            (list (make-poly (variable p1) (car res)) (make-poly (variable p2) (cadr res))))
          (error "Polys not in same var -- REDUCE-POLY"
                 (list p1 p2))))


  (define (reduce-terms term1 term2)
      (let ((gcd (gcd-terms (term-list term1) (term-list term2))))
        (display (list 'gcd gcd)) (newline)
        (let ((o1 (max (order (first-term term1)) (order (first-term term2))))
              (o2 (order (first-term gcd)))
              (c (coeff (first-term gcd))))
          (display (list 'o1 o1 'o2 o2 'c c)) (newline)
          (let ((int-fact (expt c (+ 1 o1 (- o2)))))
            (let ((nn (car
                       (div-terms (mul-term-by-all-terms (make-term 0 int-fact) term1) gcd)))
                  (dd (car
                       (div-terms (mul-term-by-all-terms (make-term 0 int-fact) term2) gcd))))
              (let ((coeff (append (map coeff term1) (map coeff term2))))
                (let ((d (gcd-list (cdr coeff) (car coeff))))
                  (display (list nn dd d)) (newline)
                  (list (car (div-terms nn (list (make-term 0 d))))
                        (car  (div-terms dd (list (make-term 0 d))))))))))))


  (define (zero?-poly p)
      (zero?-terms (term-list p)))
  (define (zero?-terms L)
      (or (empty-termlist? L)
          (and (=zero? (first-term L)) (zero?-terms (rest-terms L)))))

  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put 'negate '(polynomial)
       (lambda (p1) (tag (negate-poly p1))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) (reduce-poly p1 p2)))

  (put '=zero? '(polynomial)
       (lambda (p) (zero?-poly p)))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(install-polynomial-package)

(define (make-polynomial var terms)
    ((get 'make 'polynomial) var terms))

(define a (make-polynomial 'x
                           (list (list 5 (make-scheme-number 1)) (list 4 (make-scheme-number 2))
                                 (list 2 (make-scheme-number 3)) (list 1 (make-scheme-number -2))
                                 (list 0 (make-scheme-number -5)))))

                                        ;(mul a a)

                                        ;(define n (make-polynomial 'x '((5 1) (0 -1))))

                                        ;(define d (make-polynomial 'x '((2 1) (0 -1))))

                                        ;(display (div n d)) (newline)

                                        ;ex2.93
                                        ;(define p1 (make-polynomial 'x '((2 1) (0 1))))
                                        ;(define p2 (make-polynomial 'x '((3 1) (0 1))))
                                        ;(define rf (make-rational p2 p1))

                                        ;ex2.94
                                        ;(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
                                        ;(define p2 (make-polynomial 'x '((3 1) (1 -1))))
                                        ;(greatest-common-divisor p1 p2)

                                        ;ex2.95
                                        ;(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
                                        ;(define p2 (make-polynomial 'x '((2 11) (0 7))))
                                        ;(define p3 (make-polynomial 'x '((1 13) (0 5))))

                                        ;(define q1 (mul p1 p2))
                                        ;(define q2 (mul p1 p3))

                                        ;(greatest-common-divisor q1 q2)

(display (sumsq 1 2 3)) (newline)

(display (sumsq (make-rational 1 2) (make-rational 1 3) (make-rational 2 3)))
(newline)





















