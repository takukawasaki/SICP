
Amb評価器
;; permanet-set! 実装すみ
;; if-fail 実装すみ
;; ramb 実装すみ

;; Amb評価器

(define apply-in-underlying-scheme apply)   ;;脚注17

;;駆動ループ 4.1.4
(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment ;;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative)) ;;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '))
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
        (list 'caar caar)
        (list 'cadar cadar)
        (list 'cos cos)
        (list 'display display)
        (list 'eq? eq?)
        (list 'error error)
        ;;      (list 'eval eval)
        (list 'list list)
        (list 'list-ref list-ref)
        (list 'list-tail list-tail)
        (list 'log log)
        (list 'max max)
        (list 'member member)
        (list 'min min)
        (list 'newline newline)
        (list 'not not)
        (list 'null? null?)
        (list 'number? number?)
        (list 'pair? pair?)
        (list 'quotient quotient)
        (list 'random random)
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
        (list 'sqrt sqrt)
        (list 'floor floor)
        (list 'integer? integer?)
        (list 'inexact->exact inexact->exact)
        (list 'even? even?)
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

(define (permanent-assignment? exp)          ;;ex4.51
  (tagged-list? exp 'permanent-set!))


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


(define (if-fail? exp) (tagged-list? exp 'if-fail)) ;;ex4.52

(define (if-fail-first exp)                         ;;ex4.52
  (cadr exp))

(define (if-fail-second exp)                        ;;ex4.52
  (caddr exp))


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


;;(define (require? exp) (tagged-list? exp 'require)) ;ex4.54

;;(define (require-predicate exp) (cadr exp))         ;ex4.54

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

;; 4.1.7 eval
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ;;ex4.51
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))    ;; ex4.52
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((and? exp) (analyze (and->if exp)))     ;ex4.04
        ((or? exp) (analyze (or->if exp)))       ;ex4.04
        ((let? exp) (analyze (let->combination exp)))

        ((amb? exp) (newline) (analyze-amb exp))
        ((ramb? exp) (newline) (analyze-ramb exp))   ;;ex4.50
        ;;      ((require? exp) (newline) (analyze-require exp))   ;ex4.54

        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ;; *1*
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ;; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-permanent-assignment exp)    ;;ex4.51
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok
                        (lambda ()
                          (fail2))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))


(define (analyze-if-fail exp)                       ;;ex4.52
  (let ((fproc (analyze (if-fail-first exp)))
        (sproc (analyze (if-fail-second exp))))
    (lambda (env succeed fail)
      (fproc env succeed (lambda () (sproc env succeed fail))))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  ;;;(display (list 'as exps)) (newline)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
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
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;(define (analyze-require exp)
;;  (let ((pproc (analyze (require-predicate exp))))
;;    (lambda (env succeed fail)
;;      (pproc env
;;             (lambda (pred-value fail2)
;;               (if (not pred-value)
;;                   (fail)
;;                   (succeed 'ok fail2)))
;;             fail))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

;; ex4.50
(define (analyze-ramb exp)
  (define (list-rem list n)
    (if (= n 0) (cdr list)
        (cons (car list) (list-rem (cdr list) (- n 1)))))
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((nth (random (length choices))))
              (let ((next (list-ref choices nth))
                    (rest (list-rem choices nth)))
                (next env succeed
                      (lambda ()
                        (try-next rest)))))))
      (try-next cprocs))))

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

;;amb
(define (amb? exp) (tagged-list? exp 'amb))

;;ramb ex4.50
(define (ramb? exp) (tagged-list? exp 'ramb))

(define (amb-choices exp) (cdr exp))

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
  (cond ((null? conjuncts) 'false)
        ((null? (cdr conjuncts))
         (make-if (car conjuncts) (car conjuncts) 'false))
        (else (make-if (car conjuncts)
                       (expand-conjuncts (cdr conjuncts))
                       'false))))

(define (or->if exp)
  (expand-disjuncts (disjuncts exp)))
(define (disjuncts exp) (cdr exp))

(define (expand-disjuncts disjuncts)
  (cond ((null? disjuncts) 'true)
        ((null? (cdr disjuncts))
         (make-if (car disjuncts) (car disjuncts) 'false))
        (else (make-if (car disjuncts)
                       (car disjuncts)
                       (expand-disjuncts (cdr disjuncts))))))
;; ex4.06 ex4.08 ex4.22

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


;; 4.1.4 対話開始

(define the-global-environment (setup-environment))

(display (string #\bel))
(newline)(newline)
(display
 "CAUTION: the original apply was removed; mceval cannot be loaded again.")
(newline)
(display
 "USE (driver-loop) to return to metaciucular evaluator.")
(newline)
(driver-loop)

(define (safe? k positions)
  (display (list 'safe? k positions)) (newline)

  問題 4.36
  ;;i\j0 1 2 3  4  5
  ;; ---------------
  ;;0| 0 1 3 6 10 15
  ;;1|   2 4 7 11 ...
  ;;2|     5 8 12
  ;;3|       9 13
  ;;4|         14
  ;;
  ;;のようなf(i,j) (0<=i<=j) を考える. f(0,j)の行をみるとこれは0からjまでの和
  ;;だから f(0,j)=j(j+1)/2. したがってf(i,j)=j(j+1)/2+i.
  ;;
  ;;一方, n=f(i,j)が与えられた時, i,jを求める関数 i(n), j(n)は
  ;;i=0の時, n=j(j+1)/2だから j^2+j-2n=0からjを解いて, j(n)=(-1+sqrt(1+8n))/2
  ;;そこでi(n)=n-m(m+1)/2 ただし m=j(n).
  ;;
  ;;(define (j n)
  ;;  (inexact->exact (floor (/ (- (sqrt (+ (* 8 n) 1)) 1) 2))))
  ;;
  ;;(define (i n)
  ;;  (let ((m (j n)))
  ;;    (- n (/ (* m (+ m 1)) 2))))
  ;;
  ;;関数 (a-p-t n)はこういうnが与えられた時, nからi, jを得, k=sqrt(i*i+j*j)が
  ;;integerであれば (list i j k)を返し, そうでなければfalseを返す.
  ;;
  ;;これでnを増やしながら, i,jを作り, これがピタゴラス3角形を構成し得ることを
  ;;requireする.
  ;;
  ;;ところで関数i,jは0からの値を生成するので, 使う時はそれぞれ1を足して, 1からの
  ;;整数を使うようにしている.

  ;;このプログラムを実行するには, ambの処理系環境に
  ;;        (list 'sqrt sqrt)
  ;;        (list 'floor floor)
  ;;        (list 'integer? integer?)
  ;;        (list 'inexact->exact inexact->exact)
  ;;を追加しておかなければならない.

  (define (require p)
    (if (not p) (amb)))

  (define (an-integer-from n)
    (amb n (an-integer-from (+ n 1))))

  (define (j n)
    (inexact->exact (floor (/ (- (sqrt (+ (* 8 n) 1)) 1) 2))))

  (define (i n)
    (let ((m (j n)))
      (- n (/ (* m (+ m 1)) 2))))

  (define (a-p-t n)
    (let ((i (+ (i n) 1)) (j (+ (j n) 1)))
      (define k (sqrt (+ (* i i) (* j j))))
      (if (integer? k)
          (list i j k)
          false)))

  (define (a-pythagorean-triangle-from n)
    (let ((m (an-integer-from n)))
      (let ((result (a-p-t m)))
        (require result)
        result)))

  (a-pythagorean-triangle-from 0)

  ;;実行結果
  ;;;;; Amb-Eval input:
  ;;
  ;;(a-pythagorean-triangle-from 0)
  ;;
  ;;;;; Starting a new problem
  ;;;;; Amb-Eval value:
  ;;(3 4 5)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(6 8 10)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(5 12 13)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(9 12 15)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(8 15 17)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(12 16 20)
  ;;
  ;;;;; Amb-Eval input:
  ;;
  問題 4.37
  ;; 問題4.51 のpermanet-set! を実装してから,

  (define count 0)
  (define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high))
          (hsq (* high high)))
      (let ((j (an-integer-between i high)))
        (let ((ksq (+ (* i i) (* j j))))
          (permanent-set! count (+ count 1)) ;; 回数を数える.
          (require (>= hsq ksq))
          (let ((k (sqrt ksq)))
            (require (integer? k))
            (list i j k))))))


  (a-pythagorean-triple-between 1 20)

  ;;としてrequireの回数をしらべると, 1, 20の範囲で問題4.35は1540回, 問題4.37は
  ;;210回になる.
  問題 4.38
  (define (require p)
    (if (not p) (amb)))

  (define (abs x)
    (if (< x 0) (- x) x))

  (define (equal? x y)
    (if (pair? x)
        (if (pair? y)
            (if (equal? (car x) (car y))
                (equal? (cdr x) (cdr y))
                false)
            false)
        (eq? x y)))

  (define (member item x)
    (cond ((null? x) false)
          ((equal? item (car x)) x)
          (else (member item (cdr x)))))

  (define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))

  (define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
          (cooper (amb 1 2 3 4 5))
          (fletcher (amb 1 2 3 4 5))
          (miller (amb 1 2 3 4 5))
          (smith (amb 1 2 3 4 5)))
      (require
       (distinct? (list baker cooper fletcher miller smith)))
      (require (not (= baker 5)))
      (require (not (= cooper 1)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (> miller cooper))
      (require (not (= (abs (- smith fletcher)) 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (list (list 'baker baker)
            (list 'cooper cooper)
            (list 'fletcher fletcher)
            (list 'miller miller)
            (list 'smith smith))))

  (multiple-dwelling)
  問題 4.40
  (define (require p)
    (if (not p) (amb)))

  (define (abs x)
    (if (< x 0) (- x) x))

  (define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4))
          (cooper (amb 2 3 4 5)))
      (require (not (= baker cooper)))
      (let ((fletcher (amb 2 3 4)))
        (require (not (= baker fletcher)))
        (require (not (= cooper fletcher)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (not (= baker miller)))
          (require (not (= cooper miller)))
          (require (not (= fletcher miller)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= baker smith)))
            (require (not (= cooper smith)))
            (require (not (= fletcher smith)))
            (require (not (= miller smith)))
            (require (not
                      (= (abs (- smith fletcher)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith)))))))

  (multiple-dwelling)
  問題 4.41
                                        ;(define (equal? x y)
                                        ; (if (pair? x)
                                        ;     (if (pair? y)
                                        ;         (if (equal? (car x) (car y))
                                        ;             (equal? (cdr x) (cdr y))
                                        ;             false)
                                        ;         false)
                                        ;     (eq? x y)))
                                        ;
                                        ;(define (member item x)
                                        ;  (cond ((null? x) false)
                                        ;        ((equal? item (car x)) x)
                                        ;        (else (member item (cdr x)))))

  (define (permutations s)
    (if (null? s) (list '())
        (apply append (map (lambda (x)
                             (map (lambda (p) (cons x p))
                                  (permutations (remove x s)))) s))))

  (define (remove item sequence)
    (if (eq? item (car sequence))
        (cdr sequence)
        (cons (car sequence) (remove item (cdr sequence)))))

                                        ;(define (an-element-of items)
                                        ;  (require (not (null? items)))
                                        ;  (amb (car items) (an-element-of (cdr items))))

  (define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

  (define (multiple-dwelling)
    (let ((solutions (permutations '(1 2 3 4 5))))
      (map
       (lambda (sol)
         (let ((baker (list-ref sol 0))
               (cooper (list-ref sol 1))
               (fletcher (list-ref sol 2))
               (miller (list-ref sol 3))
               (smith (list-ref sol 4)))
           (if (and (not (= baker 5))
                    (not (= cooper 1))
                    (not (= fletcher 5))
                    (not (= fletcher 1))
                    (> miller cooper)
                                        ;                  (not (= (abs (- smith fletcher)) 1))
                    (not (= (abs (- fletcher cooper)) 1)))
               (display
                (list (list 'baker baker)
                      (list 'cooper cooper)
                      (list 'fletcher fletcher)
                      (list 'miller miller)
                      (list 'smith smith)))))) solutions)
      (newline)))

  (multiple-dwelling)


  ;;別解

  (define (multiple-dwelling)
    (define (b-iter b)
      (define (c-iter c)
        (define (f-iter f)
          (define (m-iter m)
            (define (s-iter s)
              (if (not (or (= s b) (= s c) (= s f) (= s m) (> s 5)))
                  (if (and (< b 5) (> c 1) (< f 5) (> f 1)
                           (> m c) (> (abs (- s f)) 1) (> (abs (- f c)) 1))
                      (begin (display (list b c f m s)) (newline)))
                  (s-iter (+ s 1))))
            (if (not (or (= m b) (= m c) (= m f)))
                (s-iter 1))
            (if (< m 5) (m-iter (+ m 1))))
          (if (not (or (= f b) (= f c)))
              (m-iter 1))
          (if (< f 5) (f-iter (+ f 1))))
        (if (not (or (= c b)))
            (f-iter 1))
        (if (< c 5) (c-iter (+ c 1))))
      (c-iter 1)
      (if (< b 5) (b-iter (+ b 1))))
    (b-iter 1))

  (multiple-dwelling)

  問題 4.42
  (define (require p)
    (if (not p) (amb)))

  (define (equal? x y)
    (if (pair? x)
        (if (pair? y)
            (if (equal? (car x) (car y))
                (equal? (cdr x) (cdr y))
                false)
            false)
        (eq? x y)))

  (define (member item x)
    (cond ((null? x) false)
          ((equal? item (car x)) x)
          (else (member item (cdr x)))))

  (define (remove item sequence)
    (if (eq? item (car sequence))
        (cdr sequence)
        (cons (car sequence) (remove item (cdr sequence)))))

  (define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

  (define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

  (define (liars)
    (define list0 '(1 2 3 4 5))
    (define betty (an-element-of list0))
    (define list1 (remove betty list0))
    (define ethel (an-element-of list1))
    (define list2 (remove ethel list1))
    (define joan  (an-element-of list2))
    (define list3 (remove joan list2))
    (define kitty (an-element-of list3))
    (define list4 (remove kitty list3))
    (define mary (car list4))
    ;;  (display (list betty ethel joan kitty mary)) (newline)
    (define order (list (list 'betty betty) (list 'ethel ethel)
                        (list 'joan joan) (list 'kitty kitty) (list 'mary mary)))
    (define b (amb 0 1))
    (define bt (list-ref '((kitty 2) (betty 3)) b))
    (define bf (list-ref '((kitty 2) (betty 3)) (- 1 b)))
    (require (member bt order))
    (require (not (member bf order)))
    (define e (amb 0 1))
    (define et (list-ref '((ethel 1) (joan 2)) e))
    (define ef (list-ref '((ethel 1) (joan 2)) (- 1 e)))
    (require (member et order))
    (require (not (member ef order)))
    (define j (amb 0 1))
    (define jt (list-ref '((joan 3) (ethel 5)) j))
    (define jf (list-ref '((joan 3) (ethel 5)) (- 1 j)))
    (require (member jt order))
    (require (not (member jf order)))
    (define k (amb 0 1))
    (define kt (list-ref '((kitty 2) (mary 4)) k))
    (define kf (list-ref '((kitty 2) (mary 4)) (- 1 k)))
    (require (member kt order))
    (require (not (member kf order)))
    (define m (amb 0 1))
    (define mt (list-ref '((mary 4) (betty 1)) m))
    (define mf (list-ref '((mary 4) (betty 1)) (- 1 m)))
    (require (member mt order))
    (require (not (member mf order)))
    order)

  ;; 実行例
  ;; ;;; Amb-Eval input:
  ;; (liars)
  ;;
  ;; ;;; Starting a new problem
  ;; ;;; Amb-Eval value:
  ;; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
  ;;                         1 min 20 sec
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; There are no more values of
  ;; (liars)                 1 min 10 sec
  ;;

  ;;別解 xorを使う.

  (define (xor p q) (or (and p (not q)) (and (not p) q)))
  (define (liars)
    (let ((b (amb 1 2 3 4 5))
          (e (amb 1 2 3 4 5))
          (j (amb 1 2 3 4 5))
          (k (amb 1 2 3 4 5))
          (m (amb 1 2 3 4 5)))
      (require (distinct? (list b e j k m)))
      (require (xor (= b 3) (= k 2)))
      (require (xor (= e 1) (= j 2)))
      (require (xor (= j 3) (= e 5)))
      (require (xor (= k 2) (= m 4)))
      (require (xor (= m 4) (= b 1)))
      (list b e j k m)))

  (liars)

  問題 4.43
  (define (require p)
    (if (not p) (amb)))

  (define (yacht)
    (let ((barnacle (cons 'gabrielle 'melissa))
          (moore (cons 'lorna 'mary))
          (downing (cons 'melissa
                         (amb 'gabrielle 'lorna 'rosalind)))
          (hall (cons 'rosalind
                      (amb 'gabrielle 'lorna))))
      (require (not (eq? (cdr downing) (cdr hall))))
      (let ((parker (cons 'mary
                          (amb 'gabrielle 'lorna 'rosalind))))
        (require (not (eq? (cdr downing) (cdr parker))))
        (require (not (eq? (cdr hall) (cdr parker))))
        (cond ((eq? (cdr barnacle) 'gabrielle)
               (require (eq? (car barnacle) (cdr parker))))
              ((eq? (cdr downing) 'gabrielle)
               (require (eq? (car downing) (cdr parker))))
              ((eq? (cdr hall) 'gabrielle)
               (require (eq? (car hall) (cdr parker))))
              ((eq? (cdr moore) 'gabrielle)
               (require (eq? (car moore) (cdr parker))))
              ((eq? (cdr parker) 'gabrielle)
               (require (eq? (car parker) (cdr parker)))))
        (cond ((eq? (cdr barnacle) 'lorna) 'barnacle)
              ((eq? (cdr downing) 'lorna) 'downing)
              ((eq? (cdr hall) 'lorna) 'hall)
              ((eq? (cdr moore) 'lorna) 'moore)
              ((eq? (cdr parker) 'lorna) 'parker)
              (else (amb))))))

  (yacht)

  ;; 実行例
  ;; ;;; Amb-Eval input:
  ;; (yacht)
  ;;
  ;; ;;; Starting a new problem
  ;; ;;; Amb-Eval value:
  ;; downing
  ;;
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; There are no more values of
  ;; (yacht)
  ;;

  ;;別解
  ;; 分かっている情報
  ;;Moore, Downing, Hall, Barnacle, Parkerをそれぞれmo, do, ha, ba, paと書く.
  ;;Mary, Gabrielle, Lorna, Rosalind, Melissaをそれぞれma, ga, lo, ro, meと書く.
  ;;分かっている情報を表にすると
  ;;父親 娘 ヨット
  ;;mo   ma  lo
  ;;ba   me  ga
  ;;ha       ro
  ;;do       me
  ;;pa   y   ma  (maは問題文にはないが, ユニークに決まる)
  ;;条件は
  ;;x    ga  y
  ;;z    lo
  ;;
  ;;父親 娘 ヨット
  ;;mo   ma  lo
  ;;ba   me  ga
  ;;ha (lo ga) ro    (ヨットがroなので娘にroの可能性はない)
  ;;do (ro la ga) me
  ;;pa (ro la) ma    (gaはpa以外の娘なので, gaの可能性はない)


  (define (require p)
    (if (not p) (amb)))

  (define (equal? x y)
    (if (pair? x)
        (if (pair? y)
            (if (equal? (car x) (car y))
                (equal? (cdr x) (cdr y))
                false)
            false)
        (eq? x y)))

  (define (member item x)
    (cond ((null? x) false)
          ((equal? item (car x)) x)
          (else (member item (cdr x)))))

  (define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items ) (cdr items)) false)
          (else (distinct? (cdr items)))))

  (define (ex)
    (let ((mo (list 'ma 'lo 'mo))
          (ba (list 'me 'ga 'ba))
          (ha (list (amb 'lo 'ga) 'ro 'ha))
          (do (list (amb 'ro 'lo 'ga) 'me 'do))
          (pa (list (amb 'ro 'lo) 'ma 'pa)))
      (require (distinct? (list (car ha) (car do) (car pa))))
      (let ((x (amb mo ba ha do pa)))
        (require (and (eq? (car x) 'ga) (eq? (cadr x) (car pa))))
        (let ((z (amb mo ba ha do pa)))
          (require (distinct? (list x z)))
          (require (eq? (car z) 'lo))
          z))))

  (ex)

  ;;実行例
  ;;
  ;;;;; Amb-Eval input:
  ;;(ex)
  ;;
  ;;;;; Starting a new problem
  ;;;;; Amb-Eval value:
  ;;(lo me do)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; There are no more values of
  ;;(ex)
  問題 4.44
  (define (require p)
    (if (not p) (amb)))

  (define (an-integer-between low high)
    (require (not (> low high)))
    (amb low (an-integer-between (+ low 1) high)))

  (define (safe? k positions)
    (define (safe1 x n)
      (if (< n k)
          (let ((y (list-ref positions n)))
            (require (not (= x y)))
            (require (not (= (- x y) n)))
            (require (not (= (- y x) n)))
            (safe1 x (+ n 1))) true))
    (safe1 (car positions) 1))

  (define (list-ref items n)
    (if (= n 0) (car items)
        (list-ref (cdr items) (- n 1))))

  (define (queens board-size)
    (define (queen-cols k board)
                                        ;    (display board) (newline)
      (if (> k board-size) board
          (let ((board
                 (cons (an-integer-between 1 board-size) board)))
            (safe? k board)
            (queen-cols (+ k 1) board))))
    (queen-cols 1 '()))

  ;; 実行例
  ;; ;;; Amb-Eval input:
  ;; (queens 8)
  ;;
  ;; ;;; Starting a new problem
  ;; ;;; Amb-Eval value:
  ;; (4 2 7 3 6 8 5 1)       4 min
  ;;
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; Amb-Eval value:
  ;; (5 2 4 7 3 8 6 1)       1 min 15 sec
  ;;
  問題 4.45
  (define (memq item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

  (define (require p)
    (if (not p) (amb)))

  (define nouns '(noun student professor cat class))

  (define verbs '(verb studies lectures eats sleeps))

  (define articles '(article the a))

  (define (parse-word word-list)
    (require (not (null? *unparsed*)))
    (require (memq (car *unparsed*) (cdr word-list)))
    (let ((found-word (car *unparsed*)))
      (set! *unparsed* (cdr *unparsed*))
      (list (car word-list) found-word)))

  (define *unparsed* '())

  (define (parse input)
    (set! *unparsed* input)
    (let ((sent (parse-sentence)))
      (require (null? *unparsed*))
      sent))

  (define prepositions '(prep for to in by with))

  (define (parse-prepositional-phrase)
    (list 'prep-phrase
          (parse-word prepositions)
          (parse-noun-phrase)))

  (define (parse-sentence)
    (list 'sentence
          (parse-noun-phrase)
          (parse-verb-phrase)))

  (define (parse-verb-phrase)
    (define (maybe-extend verb-phrase)
      (amb verb-phrase
           (maybe-extend (list 'verb-phrase
                               verb-phrase
                               (parse-prepositional-phrase)))))
    (maybe-extend (parse-word verbs)))

  (define (parse-simple-noun-phrase)
    (list 'simple-noun-phrase
          (parse-word articles)
          (parse-word nouns)))

  (define (parse-noun-phrase)
    (define (maybe-extend noun-phrase)
      (amb noun-phrase
           (maybe-extend (list 'noun-phrase
                               noun-phrase
                               (parse-prepositional-phrase)))))
    (maybe-extend (parse-simple-noun-phrase)))

  (parse
   '(the professor lectures to the student
         in the class with the cat))

  ;; 実行例
  ;; ;;; Amb-Eval input:
  ;;
  ;; (parse
  ;;  '(the professor lectures to the student
  ;;    in the class with the cat))
  ;;
  ;;
  ;; ;;; Starting a new problem
  ;; ;;; Amb-Eval value:
  ;;   (sentence
  ;;     (simple-noun-phrase (article the) (noun professor))
  ;;     (verb-phrase
  ;;       (verb-phrase
  ;;         (verb-phrase (verb lectures)
  ;;           (prep-phrase (prep to)
  ;;             (simple-noun-phrase (article the)
  ;;               (noun student))))
  ;;         (prep-phrase (prep in)
  ;;           (simple-noun-phrase (article the) (noun class))))
  ;;       (prep-phrase (prep with)
  ;;         (simple-noun-phrase (article the) (noun cat)))))
  ;; 教授は, 猫を連れ, 教室で, 学生に講義する
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; Amb-Eval value:
  ;;   (sentence
  ;;     (simple-noun-phrase (article the) (noun professor))
  ;;     (verb-phrase
  ;;       (verb-phrase (verb lectures)
  ;;         (prep-phrase (prep to)
  ;;           (simple-noun-phrase (article the) (noun student)))
  ;;         )
  ;;       (prep-phrase (prep in)
  ;;         (noun-phrase
  ;;           (simple-noun-phrase (article the) (noun class))
  ;;           (prep-phrase (prep with)
  ;;             (simple-noun-phrase (article the) (noun cat)))))
  ;;       ))
  ;; 教授は猫をつれた教室で, 学生に講義する
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; Amb-Eval value:
  ;;   (sentence
  ;;     (simple-noun-phrase (article the) (noun professor))
  ;;     (verb-phrase
  ;;       (verb-phrase (verb lectures)
  ;;         (prep-phrase (prep to)
  ;;           (noun-phrase
  ;;             (simple-noun-phrase (article the)
  ;;               (noun student))
  ;;             (prep-phrase (prep in)
  ;;               (simple-noun-phrase (article the)
  ;;                 (noun class))))))
  ;;       (prep-phrase (prep with)
  ;;         (simple-noun-phrase (article the) (noun cat)))))
  ;; 猫を連れた教授は, 教室にいる学生に講義する
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; Amb-Eval value:
  ;;   (sentence
  ;;     (simple-noun-phrase (article the) (noun professor))
  ;;     (verb-phrase (verb lectures)
  ;;       (prep-phrase (prep to)
  ;;         (noun-phrase
  ;;           (noun-phrase
  ;;             (simple-noun-phrase (article the)
  ;;               (noun student))
  ;;             (prep-phrase (prep in)
  ;;               (simple-noun-phrase (article the)
  ;;                 (noun class))))
  ;;           (prep-phrase (prep with)
  ;;             (simple-noun-phrase (article the) (noun cat)))))
  ;;       ))
  ;; 教授は, 猫を連れた, 教室にいる学生に講義する
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; Amb-Eval value:
  ;;   (sentence
  ;;     (simple-noun-phrase (article the) (noun professor))
  ;;     (verb-phrase (verb lectures)
  ;;       (prep-phrase (prep to)
  ;;         (noun-phrase
  ;;           (simple-noun-phrase (article the) (noun student))
  ;;           (prep-phrase (prep in)
  ;;             (noun-phrase
  ;;               (simple-noun-phrase (article the)
  ;;                 (noun class))
  ;;               (prep-phrase (prep with)
  ;;                 (simple-noun-phrase (article the)
  ;;                   (noun cat)))))))))
  ;; 教授は, (猫を連れた教室)にいる学生に講義する.
  ;; ;;; Amb-Eval input:
  ;; try-again
  ;;
  ;; ;;; There are no more values of
  ;; (parse (quote (the professor lectures to the student in the class with the cat)))
  ;;
  問題 4.50
  (define (analyze-ramb exp)
    (define (list-rem list n)
      (if (= n 0) (cdr list)
          (cons (car list) (list-rem (cdr list) (- n 1)))))
    (let ((cprocs (map analyze (amb-choices exp))))
      (lambda (env succeed fail)
        (define (try-next choices)
          (if (null? choices)
              (fail)
              (let ((nth (random (length choices))))
                (let ((next (list-ref choices nth))
                      (rest (list-rem choices nth)))
                  (next env succeed
                        (lambda ()
                          (try-next rest)))))))
        (try-next cprocs))))

  ;; この他  ((ramb? exp) (newline) (analyze-ramb exp)) を (analyze exp)に追加
  ;; (define (ramb? exp) (tagged-list? exp 'ramb))
  ;; を追加する必要あり
  ;; (load "mceval8.scm")を実行後

  (define (require p)
    (if (not p) (amb)))

  (define (ramb-test)
    (let ((a (ramb 0 1 2 3)))
      a))

  ;;実行してみる.
  ;;
  ;;(ramb-test)
  ;;
  ;;;;; Starting a new problem
  ;;;;; Amb-Eval value:
  ;;1
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;3
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;2
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;0
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; There are no more values of
  ;;(ramb-test)
  問題 4.51
  ;; 次のように追加修正する.

  ;;(define (analyze-permanent-assignment exp)    ;;ex4.51
  ;;  (let ((var (assignment-variable exp))
  ;;        (vproc (analyze (assignment-value exp))))
  ;;    (lambda (env succeed fail)
  ;;      (vproc env
  ;;             (lambda (val fail2)
  ;;                 (set-variable-value! var val env)
  ;;                 (succeed 'ok
  ;;                          (lambda ()
  ;;                            (fail2))))
  ;;             fail))))
  ;;
  ;;;; 4.1.2 代入 追加
  ;;
  ;;(define (permanent-assignment? exp)          ;;ex4.51
  ;;  (tagged-list? exp 'permanent-set!))
  ;;
  ;;;; 4.1.7 eval 修正
  ;;
  ;;(define (analyze exp)
  ;;  (cond ((self-evaluating? exp)
  ;;         (analyze-self-evaluating exp))
  ;;        ((quoted? exp) (analyze-quoted exp))
  ;;        ((variable? exp) (analyze-variable exp))
  ;;        ((assignment? exp) (analyze-assignment exp))
  ;;        ((permanent-assignment? exp) (analyzye-permanent exp))  ;;ex4.51
  ;;        ((definition? exp) (analyze-definition exp))
  ;;        ((if? exp) (analyze-if exp))
  ;;        ((lambda? exp) (analyze-lambda exp))
  ;;        ((begin? exp) (analyze-sequence (begin-actions exp)))
  ;;        ((cond? exp) (analyze (cond->if exp)))
  ;;        ((and? exp) (analyze (and->if exp)))     ;ex4.04
  ;;        ((or? exp) (analyze (or->if exp)))       ;ex4.04
  ;;        ((let? exp) (analyze (let->combination exp)))
  ;;
  ;;        ((amb? exp) (newline) (analyze-amb exp))
  ;;        ((application? exp) (analyze-application exp))
  ;;        (else
  ;;         (error "Unknown expression type -- ANALYZE" exp))))
  ;;

  (define (require p)
    (if (not p) (amb)))

  (define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items))))

  (define count 0)

  (let ((x (an-element-of '(a b c)))
        (y (an-element-of '(a b c))))
    (permanent-set! count (+ count 1))
    (require (not (eq? x y)))
    (list x y count))

  ;;実行結果
  ;;;;; Starting a new problem
  ;;;;; Amb-Eval value:
  ;;(a b 2)
  ;;
  ;;;;; Amb-Eval input:
  ;;
  ;;
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(a c 3)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(b a 4)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(b c 6)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(c a 7)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; Amb-Eval value:
  ;;(c b 8)
  ;;
  ;;;;; Amb-Eval input:
  ;;try-again
  ;;
  ;;;;; There are no more values of
  ;;(let ((x (an-element-of (quote (a b c)))) (y (an-element-of (quote (a b c))))) (permanent-set! count (+ count 1)) (require (not (eq? x y))) (list x y count))
  ;;
  ;;;;; Amb-Eval input:
  ;;
  問題 4.52
  以下を解析評価器に追加する.

  (define (if-fail? exp) (tagged-list? exp 'if-fail)) ;;ex4.52

  (define (if-fail-first exp)                         ;;ex4.52
    (cadr exp))

  (define (if-fail-second exp)                        ;;ex4.52
    (caddr exp))

  (define (analyze-if-fail exp)                       ;;ex4.52
    (let ((fproc (analyze (if-fail-first exp)))
          (sproc (analyze (if-fail-second exp))))
      (lambda (env succeed fail)
        (fproc env succeed (lambda () (sproc env succeed fail))))))

  ;;実行結果
  ;;;;; Amb-Eval input:
  ;;(define (require p)
  ;;  (if (not p) (amb)))
  ;;
  ;;;;; Starting a new problem
  ;;
  ;;;;; Amb-Eval value:
  ;;ok
  ;;
  ;;;;; Amb-Eval input:
  ;;
  ;;(define (an-element-of items)
  ;;  (require (not (null? items)))
  ;;  (amb (car items) (an-element-of (cdr items))))
  ;;
  ;;;;; Starting a new problem
  ;;
  ;;;;; Amb-Eval value:
  ;;ok
  ;;
  ;;;;; Amb-Eval input:
  ;;(if-fail (let ((x (an-element-of '(1 3 5 8))))
  ;;           (require (even? x))
  ;;           x)
  ;;         'all-odd)
  ;;
  ;;;;; Starting a new problem
  ;;;;; Amb-Eval value:
  ;;8
  ;;
  ;;;;; Amb-Eval input:
  ;;(if-fail (let ((x (an-element-of '(1 3 5))))
  ;;           (require (even? x))
  ;;           x)
  ;;         'all-odd)
  ;;
  ;;;;; Starting a new problem
  ;;;;; Amb-Eval value:
  ;;all-odd
  問題 4.53
  ;;; Amb-Eval value:
  ((8 35) (3 110) (3 20))
  問題 4.54
  (define (analyze-require exp)
    (let ((pproc (analyze (require-predicate exp))))
      (lambda (env succeed fail)
        (pproc env
               (lambda (pred-value fail2)
                 (if (not pred-value)
                     (fail)
                     (succeed 'ok fail2)))
               fail)))))
