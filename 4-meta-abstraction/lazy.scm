#!/usr/local/bin/gosh

(load "~/Dropbox/SICP/SCM/tool.scm")



(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ;;; 変更した
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ;;; 変更した
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))



(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))


(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))


(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;;thunk

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))




;;(define (evaluated-thunk? obj)
;;  (tagged-list? obj 'evaluated-thunk))
;;
;;(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))
;;
;;(define (force-it obj)
;;  (cond ((thunk? obj)
;;         (let ((result (actual-value
;;                        (thunk-exp obj)
;;                        (thunk-env obj))))
;;           (set-car! obj 'evaluated-thunk)
;;           (set-car! (cdr obj) result)  ;;;expをその値で置き換える
;;           (set-cdr! (cdr obj) '())     ;;;不要なenvを忘れる
;;           result))
;;        ((evaluated-thunk? obj)
;;         (thunk-value obj))
;;        (else obj)))


;;lazy-list
(define cdr-in-underlying-scheme cdr)
(define car-in-underlying-scheme car)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (convert x)
  (if (pair? x)
      (cons (convert (car-in-underlying-scheme x))
            (convert (cdr-in-underlying-scheme x)))
      x))

(define (list-ref items n)
  (if  (= n 0)
       (car items)
       (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-list fac1 fac2)
  (cond ((null? fac1) fac2)
        ((null? fac2) fac1)
        (else
         (cons (+ (car fac1) (car fac2))
               (add-list (cdr fac1) (cdr fac2))))))







