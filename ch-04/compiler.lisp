(defun evals (exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure  (lambda-parameters exp)
                          (lambda-body exp)
                          env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (evals (cond->if exp) env))
        ((application? exp)
         (apply (evals (operator exp) env)
                (list-of-values (operands exp) env)))
        (t
         (error "Unknown expressions type -- EVAL" exp))))

(defun applys (procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (t
         (error "Unknown procedure type --APPLY" procedure))))

(defun list-of-values (exps env)
  (if (no-operands? exps)
      nil
      (cons (evals (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
  (if (true? (evals (if-predicate exp) env))
      (evals (if-consequent exp) env)
      (evals (if-alternative exp) env)))

(defun eval-sequence (exps env)
  (cond ((last-exp? exps) (evals (first-exp exps) env))
        (t
         (evals (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
  (set-variable-value! (assignment-variable exp)
                       (evals (assignment-value exp) env)
                       env)
  'ok)

(defun eval-definition (exp env)
  (make-variable! (definition-variable exp)
                  (evals (definition-value exp) env)
                  env)
  'ok)
(defun self-evaluating? (exp)
  (cond ((numberp exp) t)
        ((stringp exp) t)
        (t nil)))

(defun variable? (exp)
  (symbolp exp))

(defun quoted? (exp)
  (tagged-list? exp 'quote))

(defun text-of-quotation (exp)
  (cadr exp))

(defun tagged-list? (exp tag)
  (if (consp exp)
      (eql (car exp) tag)))

(defun assignment? (exp)
  (tagged-list? exp 'set!))

(defun assignment-variable (exp)
  (cadr exp))

(defun assignment-value (exp)
  (caddr exp))

(defun range (start stop &optional (step 1))
  (let ((value start))
    (if (> value stop)
        nil
        (cons value (range (+ step value) stop)))))

(defun definition? (exp)
  (tagged-list? exp 'def))

(defun definition-variable (exp)
  (if (symbolp (cadr exp))
      (cadr exp)
      (caadr exp)))

(defun definition-value (exp)
  (if (symbolp (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(defun lambda? (exp)
  (tagged-list? exp 'fn))

(defun lambda-parameters (exp)
  (cadr exp))

(defun lambda-body (exp)
  (cddr exp))

(defun make-lambda (parameters body)
  (cons 'fn (cons parameters body)))

(defun if? (exp)
  (tagged-list? exp 'if#))

(defun if-predicate (exp)
  (cadr exp))

(defun if-consequent (exp)
  (caddr exp))

(defun if-alternative (exp)
  (if (not (null (cdddr exp)))
      (cadddr exp)
      'false))

(defun make-if (predicate consequent alternative)
  (list 'if# predicate consequent alternative))

(defun begin? (exp)
  (tagged-list? exp 'begin))

(defun begin-actions (exp)
  (cdr exp))

(defun last-exp? (seq)
  (null (cdr seq)))

(defun first-exp (seq)
  (car seq))

(defun rest-exp (seq)
  (cdr seq))

(defun sequence->exp (seq)
  (cond ((null seq) seq)
        ((last-exp? seq) (first-exp seq))
        (t (make-begin seq))))

(defun make-begin (seq)
  (cons 'begin seq))



(defun application? (exp)
  (consp exp))

(defun operator (exp)
  (car exp))

(defun operands (exp)
  (cdr exp))

(defun no-operands? (ops)
  (null ops))

(defun first-operand (ops)
  (car ops))

(defun rest-operands (ops)
  (cdr ops))

(defun cond? (exp)
  (tagged-list? exp 'cond))

(defun cond-clauses (exp)
  (cdr exp))

(defun cond-else-clause? (clause)
  (eq? (cond-predicate clause) 'else))

(defun cond-predicate (clause)
  (car clause))

(defun cond-actions (clause)
  (cdr clause))

(defun cond->if (exp)
  (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
  (if (null clauses)
      nil
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- cond->if" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;4-2
;;a. 最初に手続き作用をもってくると, リストになっているすべての式が手続
;;き作用に分類されてしまう. (define x 3)も手続き作用として扱われる.

;;b. (define (application? exp) (tagged-list? exp 'call))
;;を定義し, evalの中の手続き作用の処理を

;;((application? exp)
;;(let ((exp (cdr exp)))
;;(apply (eval (operator exi) env)
;;                   (list-of-values (operands exp) env))))


;;4-4
(defun and? (exp) (tagged-list? exp 'and))
(defun or? (exp) (tagged-list? exp 'or))
(defun and->if (exp)
  (expand-conjuncts (conjuncts exp)))
(defun conjuncts (exp) (cdr exp))

(defun expand-conjuncts (conjuncts)
  (cond ((null? conjuncts) 'true)
        ((null? (cdr conjuncts))
         (make-if (car conjuncts) (car conjuncts) 'false))
        (else (make-if (car conjuncts)
                       (expand-conjuncts (cdr conjuncts))
                       'false))))

(defun or->if (exp)
  (expand-disjuncts (disjuncts exp)))
(defun disjuncts (exp) (cdr exp))

(defun expand-disjuncts (disjuncts)
  (cond ((null? disjuncts) 'false)
        ((null? (cdr disjuncts))
         (make-if (car disjuncts) (car disjuncts) 'false))
        (else (make-if (car disjuncts)
                       (car disjuncts)
                       (expand-disjuncts (cdr disjuncts))))))

;;4-5


(defun expand-clauses (clauses)
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


;;4-6

(defun make-lambda (parameters body)
  (cons 'lambda (cons parameters body)))

(defun let? (exp) (tagged-list? exp 'let))

(defun let->combination (exp)
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (princ bindings) (fresh-line)
    (princ body) (fresh-line)
    (cons (make-lambda (mapcar car bindings) body)
          (mapcar cadr bindings))))



;;4-7

(defun make-lambda (parameters body)
  (princ parameters) (fresh-line)
  (princ body) (fresh-line)
  (cons 'lambda (cons parameters body)))

(defun let*? (exp) (tagged-list? exp 'let*))

(defun let*->nested-lets (exp)
  (defun (nested-lets bindings body)
      (if (null? (cdr bindings))
          (cons (make-lambda (list (caar bindings)) body) (cdar bindings))
          (cons (make-lambda (list (caar bindings))
                             (list (nested-lets (cdr bindings) body))) (cdar bindings))))
  (let ((bindings (cadr exp)) (body (cddr exp)))
    (nested-lets bindings body)))

;;(let*->nested-lets '(let* ((a b) (c d) (e f)) g h i j))


;;4-12


(defun env-loop (var env hitproc emptyproc err)
    (define (scan vars vals)
        (cond ((null? vars) (emptyproc env))
              ((eq? var (car vars)) (hitproc vals))
              (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment) (error err var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(defun define-variable! (var val env)
    (env-loop var
              env
              (lambda (vals) (set-car! vals val))
              (lambda (env) (add-binding-to-frame! var val (first-frame env)))
              ""))

(defun set-variable-value! (var val env)
    (env-loop var
              env
              (lambda (vals) (set-car! vals val))
              (lambda (env)
                (set-variable-value! var val (enclosing-environment env)))
              "Unbound variable -- SET!"))

(defun lookup-variable-value (var env)
    (env-loop var
              env
              (lambda (vals) (car vals))
              (lambda (env)
                (lookup-variable-value var (enclosing-environment env)))
              "Unbound variable"))

;;4.13

(defun unbind! (var env)
    (let ((frame (first-frame env)))
      (defun scan (vars vals)
          (cond ((null? vars))
                ((eq? var (car vars)) (set-car! vars '()))
                (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))

(defun first-frame (env) (car env))
(defun frame-variables (frame) (car frame))
(defun frame-values (frame) (cdr frame))

(defvar foo '(a b c d))
(defvar bar '(0 1 2 3))
(defvar env (cons (cons foo bar) '()))

(unbind! 'c env)
env



(defstruct city name long lat)

(defvar cities nil)

(defparameter u
  '((sanfarn 0 0) (okka 0 0) (ll 0 0) (pp 0 0)
    (aa 0 0) (hdh 0 0) (jk 0 0) (asss 0 0)))


(defun make-cities (x &optional (city cities))
  (let ((a-city (first x))
        (rest-city (rest x)))
    (if (null a-city)
        nil
        (setf cities (cons (make-city :name (index 0 a-city)
                                      :long (index 1 a-city)
                                      :lat  (index 2 a-city))
                           (make-cities rest-city cities))))))


(defun index (pos L)
  (nth pos L))


(defun searching (name &optional (L cities))
  (city-namep  name L))


(defun city-namep (name L)
  (car (remove-if #'null (mapcar #'(lambda (x)
                                     (when (eql (city-name x) name)
                                       x))
                                 L))))





(defun update-cities (x &optional (city cities))
  (let ((a-city (first x))
        (rest-city (rest x)))
    (if (null a-city)
        nil
        (setf cities (append cities
                             (list (make-city :name (index 0 a-city)
                                              :long (index 1 a-city)
                                              :lat (index 2 a-city)))
                             (update-cities rest-city cities))))))



