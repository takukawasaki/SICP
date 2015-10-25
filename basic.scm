;; #?= をつけるとトレースできる gauche
(use srfi-27);;random
(use srfi-106);;networking
(use srfi-1)
(use srfi-11) ;; 多値
(use util.match)
(use gauche.test)
(load "./tool.scm")

(define nil '())
(define true #t)
(define false #f)
(define fn lambda)

  
(define (tail L)
  (cond ((eq? (length  L) 1) (car L))
        (else (tail (cdr L)))))



(define (nest x y)
  (for-each (fn (i)
                (for-each (fn (j)
                              (print i " > " j)) y))
            x))


(define (nest-map x y)
  (accumulator append
          nil
          #?=(map (fn (i)
                      #?=(map (fn (j)
                                  #?=(list i j)) y))
                  x)))


(define (nest-m x y)
  #?=(map (fn (i)
              apply #?=(map (fn (j)
                          #?=(list i j)) y))
          x))


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


(define (kk :optional (a 2) (c 4):key (b 3))
  (list a c b))



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



(define (fmap proc seq)
  (accumulator append
          nil
          (proc seq)))
            



(define (nest-map x y)
  (flatmap (fn (i)
               (map (fn (j)
                        (list i j))
                    (range 1 3)))
           (range 0 2)))


;; 可変引数

(define (append/log . args)
  (print "args=" args)
  (apply append args))

(define (make-logger func)
  (lambda args
    (print "args=" args)
    (apply func args)))

(define (append2 a b)
  (if (pair? a)
      (cons (car a) (append2 (cdr a) b))
      b))


(define (append . args)
  (match args
    (() nil)
    ((a) a)
    ((a . b) (append2 a (apply append b)))))


(define (proc x . args)
  (let-optionals* args ((a 'a)
                        (b 'b)
                        (c 'c))
                  (list x a b c)))
;;(proc 0) -> '(0 a b c)
;;(proc 1 2 ) -> '(1 2  b c )


(define (proc2 . args)
  (let-optionals* args ((a 'a) . b)
                  (list a b)))
;;(proc2) -> '(a  ())
;;(proc2 0 1) -> '(0 (1))

(define (proc3 . args)
  (let-optionals* args ((a 0)
                        (b (+ a 1))
                        (c (+ b 1)))
                  (list a b c)))
;;(proc3)          ⇒ (0 1 2)
;;(proc3 8)        ⇒ (8 9 10)
;;(proc3 8 2)      ⇒ (8 2 3)
;;(proc3 8 2 -1)   ⇒ (8 2 -1)


(define (person . args)
  (let-keywords args ((name "anonym")
                      (age "unknown"))
                (print name "is " age " years old")))
                


(find (lambda (num) (< num 3)) '(5 2 3))
(find  (cut < <> 3) '(2 3 1 4))

;;多値
(min&max 1 2 3 4 5 6 7 89)
(split-at '(1 2 3 4 5 5) 3)
(receive (min max)
    (min&max 3 4 5 1)
  (list min max))

(receive (min . rest)
    (min&max 3 4 5 1)
  (list min rest))

(receive all
    (min&max 3 4 5 1)
  all)




(let-values (((min-val max-val) (min&max 1 2 3 4 4)))
  (format #t "max: ~s\nmin: ~s\n" max-val min-val))

(let ((a 2)
      (b 3))
  (let-values (((a b) (min&max 0 100))
               ((x y) (min&max a b)))
    (format #t "x: ~s\ny: ~s\n" x y))) ;;out-side a b

(let ((a 2)
      (b 3))
  (let*-values (((a b) (min&max 0 100))
               ((x y) (min&max a b)))
    (format #t "x: ~s\ny: ~s\n" x y))) ;;inside a b

(values-ref (min&max 0 3 -1) 1)
(values 1 2 3 4)


(char=? #\a #\a)
(string=? "abc" "abc")

(char-ci=? #\a #\A) ;;case insensitive
(string-ci=? "abc" "ABC")
(use util.isomorph)

(define (draw code)
  (case code
    ((1 2) #t )
    ((3) #f)
    (else #t)))


(define *inventory* (list 'potion 'potion 'dagger 'cookie 'dagger))

(member 'cookie *inventory*)

(define (member2 elt lis . opt)
  (let-optionals* opt ((cmp-fn equal?))
                  (define (loop lis )
                    (cond ((null? lis) #f)
                          ((cmp-fn elt (car lis)) lis)
                          (else (loop (cdr lis)))))
                  (loop lis)))

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


(push! *inventory* 3)



(define (traverse init get-key return-fn repeat)
  (lambda (elt lis . options)
    (let-optionals* options ((cmp-fn equal?))
                    (define (loop lis)
                      (cond ((null? lis) init)
                            ((cmp-fn elt (get-key lis)) (return-fn lis))
                            (else (repeat loop lis))))
                    (loop lis))))
(define member2
  (traverse #f car values
            (lambda(loop lis) (loop (cdr lis)))))


(define *item*
  '((potion (drink . #t) (throw . #t))
    (elixir (drink . #t) (throw . #t))
    (pancake (eat . #t) (throw . #t))
    (cookie (eat . #t) (throw . #t))
    (dagger (throw . #t))))

(define (item-properties itm)
  (cond ((assoc itm *item*) => cdr)
        (else '())))

(define (add :optional (a 10) :rest b)
  (apply + a b))


(define (item-properties-get item prop)
  (cond ((assoc prop (item-properties item)) => cdr)
        (else #f)))

(define (run-last-test p n)
  (define (loop seq i)
    (unless (>= i n)
      (test* (format "last-pair ~s" i) p (last-pair seq))
      (loop (cons i seq) (+ i 1))))
  (loop p 0))


;;test
(test-record-file "test")
(test-start "mytest.scm")
(load "./mytest")

(test* "last-pair #1" '(3) (last-pair '(1 2 3)))
(test* "last-pair #2" '(1) (last-pair '(1)))
(test* "last-pair #3" '(2 . 3) (last-pair '(1 2 . 3)))
(run-last-test '(a) 10)
(test* "last-pair error" *test-error* (last-pair '()))


(test-end)


;;try - error

;;(guard (e (else (print ">>> error <<<") (raise e)))
  ;;     (car 1))

(define (read-e )
  (guard (e
          ((<io-error> e) "io error")
          ((<read-error> e) "read-error")
          (else "other error"))
         (read)))

