#!/usr/local/bin/gosh

(define (length list)
  (if (null? list)
      0
      (+ 1 (length (cdr list)))))



(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;末尾再帰版

(define (length list)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ count 1))))
  (length-iter list 0))


;;2-18
(define (reverse l)
  (if (null? l)
      '()
      (append (reverse (cdr l)) (list (car l)))))

;;末尾再帰版
(define (my-reverse LIsT)
  (define (reverse-iter l a)
    (if (null? l)
        a
        (reverse-iter (cdr l) (cons (car l) a))))
  (reverse-iter LIsT '()))

