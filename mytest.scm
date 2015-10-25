#!/usr/local/bin/gosh

(define (last-pair seq)
  (if (pair? (cdr seq))
      (if (null? (cdr seq))
          seq
          (last-pair (cdr seq)))
      (error "last-pair needs a pair , but got: " seq)))


          

