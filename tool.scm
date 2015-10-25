;; #?= をつけるとトレースできる gauche
(use srfi-27)
(use util.match);; 
(use gauche.sequence);;シーケンスフレームワーク
(use gauche.test)
(define nil '())
(define true #t)
(define false #f)
(define fn lambda)

  
(define (runtime)
  (use srfi-11)
  (let-values (((a b) (sys-gettimeofday)))
    (+ (* a 1000000) b)))


(define-macro (delay x) `(lambda () ,x))
(define user-initial-environment interaction-environment)


(define (tree-walk walker proc tree)
  (walker (fn (elt)
              (if (list? elt)
                  (tree-walk walker proc elt)
                  (proc elt)))
          tree))


(define (filter pred list)
  (cond ((null? list) nil)
        ((pred (car list))
         (cons (car list)
               (filter pred (cdr list))))
        (else
         (filter pred (cdr list)))))

(define (filter-display pred  seq)
  (for-each display
            (filter pred seq)))




(define (accumulator op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulator op initial (cdr seq)))))

(define (range low high :optional (step 1))
  (if (>= low high)
      nil
      (cons low
            (range (+ low step)
                   high
                   step))))




(define (enum-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else
         (append (enum-tree (car tree))
                 (enum-tree (cdr tree))))))

(define (flatmap proc seq)
  (accumulator append
          nil
          (map proc seq)))



(define (nest-map x y)
  (flatmap (fn (i)
               (map (fn (j)
                        (list i j))
                    (range 1 3)))
           (range 0 2)))



(define (flatten seq)
  (define (flat s acc)
    (cond ((null? s) acc)
          ((not (pair? s)) (cons s acc))
          (else
           (flat (car s)
                 (flat (cdr s) acc)))))
  (flat seq nil))


(define (delete-all elt lis . opt)
  (let-optionals* opt ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) '())
                          ((cmp-fn elt (car lis)) (loop (cdr lis)))
                          (else (cons (car lis) (loop (cdr lis))))))
                  (loop lis)))

(define (delete-1 elt lis . opt)
  (let-optionals* opt ((cmp-fn equal?))
                  (define (loop lis)
                    (cond ((null? lis) '())
                          ((cmp-fn elt (car lis))  (cdr lis))
                          (else (cons (car lis) (loop (cdr lis))))))
                  (loop lis)))



(define (traverse init get-key return-fn repeat)
  (lambda (elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
                    (define (loop lis)
                      (cond ((null? lis) init)
                            ((cmp-fn elt (get-key lis)) (return-fn lis))
                            (else (repeat loop lis))))
                    (loop lis))))
(define (list-ref items n)
  (if  (= n 0)
       (car items)
       (list-ref (cdr items) (- n 1))))


(define (nearly-equal? a b)
  (< (abs (- a b)) 1.0e-11))

