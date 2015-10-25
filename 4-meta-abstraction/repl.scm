#!/usr/local/bin/gosh

(load "~/Dropbox/SICP/SCM/tool.scm")


;;4-1

                                        ;left-to-right
(define (list-of-values exp env)
  (if (no-operands? exp)
      '()
      (let ((val (evaltest (first-operand exp) '())))
        (cons val (list-of-values (rest-operands exp) '())))))

                                        ;right-to-left
(define (list-of-values exp env)
  (if (no-operands? exp)
      '()
      (let ((val (list-of-values (rest-operands exp) env)))
        (cons (evaltest (first-operand exp) '()) val))))



;;4-3
;;4-4

(define (and? exp) (tagged-list? exp 'and))

(define (or? exp) (tagged-list? exp 'or))

(define (and->if exp)
  (expand-conjuncts (conjuncts exp)))

(define (conjuncts exp) (cdr exp))

(define (expand-conjuncts conjuncts)
  (cond ((null? conjuncts) 'true)
        ((null? (cdr conjuncts))
         (make-if (car conjuncts) (car conjuncts) 'false))
        (else (make-if (car conjuncts)
                       (expand-conjuncts (cdr conjuncts))
                       'false))))


(define (or->if exp)
  (expand-disjuncts (disjuncts exp)))

(define (disjuncts exp) (cdr exp))

(define (expand-disjuncts disjuncts)
  (cond ((null? disjuncts) 'false)
        ((null? (cdr disjuncts))
         (make-if (car disjuncts) (car disjuncts) 'false))
        (else (make-if (car disjuncts)
                       (car disjuncts)
                       (expand-disjuncts (cdr disjuncts))))))


;;4-5

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                           ;no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (eq? (cadr first) '=>)                                   ;ex4.05
            (let ((temp `(let ((val ,(cond-predicate first)))       ;ex4.05
                           (if val (,(caddr first) val)                        
                               ,(expand-clauses rest)))))
              (display temp) (newline) temp)
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

;;4-6

(define (let? exp) (tagged-list? exp 'let))

(define (let-combination exp)
  (let ((bindings (cadr exp))
        (body (cddr exp)))
    (display bindings) (newline)
    (display body)(newline)
    (cons (make-lambda (map car bindings) body)
          (map cadr bindings))))


;;4-7


(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (nested-lets bindings body)
    (if (null? (cdr bindings))
        (cons (make-lambda (list (caar bindings)) body) (cdar bindings))
        (cons (make-lambda (list (caar bindings))
                           (list (nested-lets (cdr bindings) body))) (cdar bindings))))
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (nested-lets bindings body)))


;;4-8


(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let ((tag (cadr exp)) (bindings (caddr exp)) (body (cdddr exp)))
        (list (list 'lambda '()
                    (cons 'define (cons (cons tag (map car bindings)) body))
                    (cons tag (map cadr bindings)))))
      (let ((bindings (cadr exp)) (body (cddr exp)))
        (cons (make-lambda (map car bindings) body)
              (map cadr bindings)))))


;;4-12


(define (env-loop var env hitproc emptyproc err)
  (define (scan vars vals)
    (cond ((null? vars) (emptyproc env))
          ((eq? var (car vars)) (hitproc vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment) (error err var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (define-variable! var val env)
  (env-loop var
            env
            (lambda (vals) (set-car! vals val))
            (lambda (env) (add-binding-to-frame! var val (first-frame env)))
            ""))

(define (set-variable-value! var val env)
  (env-loop var
            env
            (lambda (vals) (set-car! vals val))
            (lambda (env)
              (set-variable-value! var val (enclosing-environment env)))
            "Unbound variable -- SET!"))

(define (lookup-variable-value var env)
  (env-loop var
            env
            (lambda (vals) (car vals))
            (lambda (env)
              (lookup-variable-value var (enclosing-environment env)))
            "Unbound variable"))


;;4-13

(define (unbind! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars))
            ((eq? var (car vars)) (set-car! vars '()))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))





;;駆動ループ 4.1.4
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;環境

(define primitive-procedures
  (list (list '* *)
        (list '+ +)
        (list '- -)
        (list '/ /)
        (list '< <)
        (list '= =)
        (list '> >)
        ;;      (list 'apply apply)
        (list 'assoc assoc)
        (list 'atan atan)
        (list 'cadr cadr)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'cos cos)
        (list 'display display)
        (list 'eq? eq?)
        (list 'error error)
        ;;      (list 'eval eval)
        (list 'list list)
        (list 'log log)
        (list 'max max)
        (list 'min min)
        (list 'newline newline)
        (list 'not not)
        (list 'null? null?)
        (list 'number? number?)
        (list 'pair? pair?)
        (list 'quotient quotient)
        (list 'read read)
        (list 'remainder remainder)
        (list 'round round)
        (list 'runtime runtime)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'sin sin)
        (list 'symbol? symbol?)
        (list 'vector-ref vector-ref)
        (list 'vector-set! vector-set!)
        ))

(define the-empty-environment '())

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (make-frame variables values)
  (cons variables values))


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (first-frame env) (car env))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))


(define (enclosing-environment env) (cdr env))



(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

;; 4.1.2 evalの下請け手続き

;; 4.1.2 自己評価式
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 4.1.2 変数
(define (variable? exp) (symbol? exp))

;; 4.1.2 クォート式
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; 4.1.2 代入
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; 4.1.2 定義
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ;formal parameter
                   (cddr exp)))) ;body

;; 4.1.2 lambda式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; 4.1.2 条件式
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; 4.1.2 begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; 4.1.2 手続き作用
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; 4.1.2 導出された式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


(define (expand-clauses clauses)
  (if (null? clauses)
      'false                           ;no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (eq? (cadr first) '=>)                                   ;ex4.05
            (let ((temp `(let ((val ,(cond-predicate first)))       ;ex4.05
                           (if val (,(caddr first) val)                         ;ex4.05
                               ,(expand-clauses rest)))))
              (display temp) (newline) temp)
            (if (cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last -- COND->IF"
                           clauses))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

                                        ;(define (expand-clauses clauses)
                                        ;  (if (null? clauses)
                                        ;      'false                           ;no else clause
                                        ;      (let ((first (car clauses))
                                        ;            (rest (cdr clauses)))
                                        ;        (if (cond-else-clause? first)
                                        ;            (if (null? rest)
                                        ;                (sequence->exp (cond-actions first))
                                        ;                (error "ELSE clause isn't last -- COND->IF"
                                        ;                       clauses))
                                        ;            (make-if (cond-predicate first)
                                        ;                     (sequence->exp (cond-actions first))
                                        ;                     (expand-clauses rest))))))

;; 4.1.1 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; 4.1.1 条件式
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; 4.1.1 並び
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; 4.1.1 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;; 4.1.1 eval

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))

        ((and? exp) (eval (and->if exp) env))     ;ex4.04
        ((or? exp) (eval (or->if exp) env))       ;ex4.04

        ((let? exp) (eval (let->combination exp) env))  ;ex4.06
        ((let*? exp) (eval (let*->nested-lets exp) env))  ;ex4.07
        ((letrec? exp) (eval (letrec->let exp) env))    ;ex4.20

        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; 4.1.1 apply

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


;;; representing procedures

;; 4.1.3 条件のテスト
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; 4.1.3 手続きの表現
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; 4.1.3 環境に対する操作

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;;ex4.04
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and->if exp)
  (expand-conjuncts (conjuncts exp)))
(define (conjuncts exp) (cdr exp))

(define (expand-conjuncts conjuncts)
  (cond ((null? conjuncts) 'true)
        ((null? (cdr conjuncts))
         (make-if (car conjuncts) (car conjuncts) 'false))
        (else (make-if (car conjuncts)
                       (expand-conjuncts (cdr conjuncts))
                       'false))))

(define (or->if exp)
  (expand-disjuncts (disjuncts exp)))
(define (disjuncts exp) (cdr exp))

(define (expand-disjuncts disjuncts)
  (cond ((null? disjuncts) 'false)
        ((null? (cdr disjuncts))
         (make-if (car disjuncts) (car disjuncts) 'false))
        (else (make-if (car disjuncts)
                       (car disjuncts)
                       (expand-disjuncts (cdr disjuncts))))))
;; ex4.06 ex4.08

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let ((tag (cadr exp)) (bindings (caddr exp)) (body (cdddr exp)))   ;ex4.08
        (list (list 'lambda '()                                         ;ex4.08
                    (cons 'define (cons (cons tag (map car bindings)) body));ex4.08
                    (cons tag (map cadr bindings)))))                      ;ex4.08
      (let ((bindings (cadr exp)) (body (cddr exp)))
        (cons (make-lambda (map car bindings) body)
              (map cadr bindings)))))


;; ex4.07

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (nested-lets bindings body)
    (if (null? (cdr bindings))
        (cons (make-lambda (list (caar bindings)) body) (cdar bindings))
        (cons (make-lambda (list (caar bindings))
                           (list (nested-lets (cdr bindings) body))) (cdar bindings))))
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (nested-lets bindings body)))

;; ex4.20
(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec->let exp)
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (cons 'let (cons
                (map (lambda (x) (list (car x) ''())) bindings)
                (append (map (lambda (x) (cons 'set! x)) bindings) body)))))


(define the-global-environment (setup-environment))

;;4-16

(define (scan-out-defines procedure-body)
  (let ((lets '())
        (sets '()))
    (define (scan body)
      (cond ((tagged-list? (car body) 'define)
             (set! lets (cons (list (cadar body) '*unassigned) lets))
             (set! sets (cons (cons 'set! (cdar body)) sets))
             (scan (cdr body)))
            (else body)))
    (display "scan-out entered") (newline)
    (define body (scan procedure-body))
    (display body) (newline)
    (cons 'let
          (cons (reverse lets)
                (append (reverse sets) body)))))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define body
  '((define even?
      (lambda (n)
        (if (= n 0)
            true
            (odd? (- n 1)))))
    (define odd?
      (lambda (n)
        (if (= n 0)
            false
            (even? (- n 1)))))
    (even? x)))




;; 4.1.7 eval
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

