
;; 5.2
(controller
  (assign p (const 1))
  (assign c (const 1))
test-c
  (test (op >) (reg c) (reg n))
  (branch (label factorial-done))
  (assign t (op *) (reg c) (reg p))
  (assign p (reg t))
  (assign s (op +) (reg c) (const 1))
  (assign c (reg s))
  (goto (label test-c))
factorial-done)



;; 5.3
(controller
 square-root-loop
  (assign x (op read))
  (assing guess (const 1.0))
  test
  (assign a (op square) (reg x))
  (assign b (op -) (reg a) (reg x))
  (assing c (op abs) (reg b))
  (test (op <) (reg c) (const 0.001)))
  (branch (label square-root-done))
  (assign d (op /) (reg x) (reg quess))
  (assign e (op +) (reg guess) (reg d))
  (assign guess (op /) (reg e) (const 2))
  (goto (label test))

square-root-done
  (perform (op print) (reg guess))
  (goto (lable square-root-loop)))



;; 5.4
a
(controller
  (assign continue (label expt-done))
expt-loop
  (test (op =) (reg n) (const 0))
  (branch (label return))
  (save continue)
  (assign continue (label after-expt))
  (assign n (op -) (reg n) (const 1))
  (goto (label expt-loop))
after-expt
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
return
   (assign val (const 1))
   (goto (reg continue))
expt-done)

b
(controller
  (assign counter (reg n))
  (assign product (const 1))
expt-loop
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg b) (reg product))
  (goto (label expt-loop))
expt-done)

;;5.7参照

;; 5.6
図5.12 「;;Fib(n-2)を計算するよう設定」の前後の
restore continue と
save continue


;;レジスタ計算機シミュレータ 

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;;レジスタ p.308

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;;スタック p.308

;; p.308のmake-stackをスタック使用統計監視用make-stackで置き換えた p.318

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;基本計算機 p.309

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
             (list (list 'initialize-stack
                         (lambda () (stack 'initialize)))
                   (list 'print-stack-statistics                  ;スタック使用
                         (lambda () (stack 'print-statistics))))) ;統計監視用
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;アセンブラ p.310

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;;命令の実行手続きの生成 p.313

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))


;;assign命令 p.313

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

;;test, branchおよびgoto命令 p.314

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;その他の命令 p.315

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;;部分式の実行手続き p.315

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;;抽象構文用手続き p.218

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(load "regmacheceval2.sch")

;;デバッグ支援レジスタ計算機シミュレータ
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*) (trace-flag '()))  ;;ex5.18
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
              (if trace-flag   ;ex5.18
                (begin (newline) (display "register name:") (display name)
                       (newline) (display "old content:") (display contents)
                       (newline) (display "new content:") (display value)))
              (set! contents value)))
            ((eq? message 'trace-on) (set! trace-flag #t)) ;ex5.18
            ((eq? message 'trace-off) (set! trace-falg '())) ;ex5.18
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

;; スタック使用統計監視用make-stack
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (get-db machine)    ;ex5.12
  (machine 'get-db))

(define (reset-inst-count machine) ;ex5.15
  (machine 'reset-inst-count))

(define (get-inst-count machine) ;ex5.15
  (machine 'get-inst-count))

(define (set-register-trace! machine register-name)  ;ex5.18
  (set-trace (get-register machine register-name)))

(define (reset-register-trace! machine register-name)  ;ex5.18
  (reset-trace (get-register machine register-name)))

(define (set-trace register)  ;ex5.18
  (register 'trace-on))

(define (reset-trace register) ;ex5.18
   (register 'trace-off))

(define (set-breakpoint machine label n) ;ex5.19
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n) ;ex5.19
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine) ;ex5.19
  (machine 'cancel-all-breakpoints))

(define (proceed-machine machine) ;ex5.19
  (machine 'proceed-machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (the-labels '()) ;ex5.19
        (trace-flag '()) ;ex5.16
        (inst-count 0) ;ex5.15
        (db '()))      ;ex5.12
    (let ((the-ops
             (list (list 'initialize-stack
                         (lambda () (stack 'initialize)))
                   (list 'print-stack-statistics
                         (lambda () (stack 'print-statistics)))
                   (list 'trace-on  ;ex5.16
                         (lambda () (set! trace-flag #t)))
                   (list 'trace-off ;ex5.16
                         (lambda () (set! trace-flag '())))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (init-db)
               (set! db (map (lambda (x) (list x)) 
                   (list 'assign 'test 'branch 'goto 'save 'restore 'perform
                         'label-reg 'save-reg 'restore-reg 'source)))
               'done)
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (if (pair? (caaar insts)) (cdaaar insts) ;ex5.19

              (begin
                (if trace-flag
                 (cond ((eq? (caaar insts) 'label)
                        (newline) (display (cadaar insts)) (display ":"))
                       (else (newline) (display (caar insts)))))
                (if (not (eq? (caaar insts) 'label))
                    (set! inst-count (+ inst-count 1)))
                ((instruction-execution-proc (car insts)))
                 (execute))))))

      (define (set-breakpoint label n)  ;ex5.19 設定
        (let ((insts (lookup-label the-labels label))
          (breakpoint-id (list 'breakpoint label n)))
        (define (insert-breakpoint insts n) ;設定の下請け ループ
          (cond ((null? insts) (error "breakpoint too far -- SETBREAKPOINT" n))
                ((eq? (caaar insts) 'label) (insert-breakpoint (cdr insts) n))
                ((> n 0) (insert-breakpoint (cdr insts) (- n 1)))
                (else (set-car! (caar insts) 
                                (cons (caaar insts) breakpoint-id)))))
        (insert-breakpoint insts n)
       'done))

      (define (cancel-breakpoint label n)   ;ex5.19 削除
        (let ((insts (lookup-label the-labels label)))
       (define (remove-breakpoint insts n)  ;削除の下請け ループ
         (cond ((null? insts) 
                (error "breakpoint too far -- CANCELBREAKPOINT" n))
               ((eq? (caaar insts) 'label) (remove-breakpoint (cdr insts) n))
               ((> n 0) (remove-breakpoint (cdr insts) (- n 1)))
               ((pair? (caaar insts))
                (set-car! (caar insts) (caaaar insts)))))
       (remove-breakpoint insts n)
        'done))

      (define (reset insts)       ;ex5.19 ブレークポイント全部削除 
        (if (pair? insts)
          (begin
          (if (pair? (caaar insts))
                     (set-car! (caar insts) (caaaar insts)))
          (reset (cdr insts)))
          'done))

      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'init-db) (init-db)) ;ex5.12
              ((eq? message 'get-db) db) ;ex5.12


              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence (car seq))
                             (set! the-labels (cdr seq))))  ;ex5.19

              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'get-inst-count) inst-count) ;ex5.15
              ((eq? message 'reset-inst-count) (set! inst-count 0) 'ok);ex5.15
              ((eq? message 'set-breakpoint)            ;ex5.19
               (lambda (label n) (set-breakpoint label n)))
              ((eq? message 'cancel-breakpoint)         ;ex5.19
               (lambda (label n) (cancel-breakpoint label n)))
              ((eq? message 'cancel-all-breakpoints)    ;ex5.19
               (reset the-instruction-sequence))
              ((eq? message 'proceed-machine)           ;ex5.19
               ((instruction-execution-proc (car (get-contents pc))))
               (execute))
              (else (error "Unknown request -- MACHINE" message))))
     (init-db)     ;ex5.12 databaseの初期化
      dispatch)))

;;assembly program

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      (cons insts labels)))) ;ex5.19

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
             (let ((insts (cons (list (list 'label next-inst)) insts)));;ex5.17
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

(define (insert! value record)
  (if (not (member value (cdr record)))
      (set-cdr! record (cons value (cdr record)))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'label)      ;;ex5.17
         (lambda () (advance-pc pc)))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (insert! inst (assoc 'assign (machine 'get-db)))
    (let ((reg (assign-reg-name inst))
          (val (assign-value-exp inst))
          (record (assoc 'source (machine 'get-db))))
;;(newline) (display record)
     (if (assoc reg (cdr record))
        (insert! val (assoc reg (cdr record)))
        (set-cdr! record (cons (list reg val) (cdr record)))))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (insert! inst (assoc 'test (machine 'get-db)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (insert! inst (assoc 'branch (machine 'get-db)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (insert! inst (assoc 'goto (machine 'get-db)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (insert! (register-exp-reg dest) 
                    (assoc 'label-reg (machine 'get-db)))
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (insert! inst (assoc 'save (machine 'get-db)))
    (insert! (stack-inst-reg-name inst)
                  (assoc 'save-reg (machine 'get-db)))
    (lambda ()
;;      (display (list 'push reg)) (newline)
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (insert! inst (assoc 'restore (machine 'get-db)))
    (insert! (stack-inst-reg-name inst)
                  (assoc 'restore-reg (machine 'get-db)))
    (lambda ()
;;      (display (list 'pop reg)) (newline)
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (insert! inst (assoc 'perform (machine 'get-db)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))



(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
問題 5.7

;;再帰版
(define expt-machine
  (make-machine
   '(b n val continue)
   (list (list '- -) (list '* *) (list '= =))
   '((assign continue (label expt-done))
   expt-loop
     (assign val (const 1))
     (test (op =) (reg n) (const 0))
     (branch (label return))
     (save continue)
     (assign continue (label after-expt))
     (assign n (op -) (reg n) (const 1))
     (goto (label expt-loop))
  after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
  return
     (assign val (const 1))
     (goto (reg continue))
  expt-done)))

(set-register-contents! expt-machine 'b 2)

(set-register-contents! expt-machine 'n 8)

(start expt-machine)

(get-register-contents expt-machine 'val)

;;反復版
(define expt-machine
  (make-machine
   '(b n counter product)
   (list (list '- -) (list '* *) (list '= =))
   '((assign counter (reg n))
     (assign product (const 1))
   expt-loop
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt-loop))
  expt-done)))

(set-register-contents! expt-machine 'b 2)

(set-register-contents! expt-machine 'n 8)

(start expt-machine)

(get-register-contents expt-machine 'product)
問題 5.8
(define ex5.8machine
  (make-machine
 '(a)
 '()
 '(start
    (goto (label here))
   here
    (assign a (const 3))
    (goto (label there))
   here
    (assign a (const 4))
    (goto (label there))
   there)))

(start ex5.8machine)
(get-register-contents ex5.8machine 'a)
;; 実行してみると -> 3 

;; labelをlabelsに追加するときに，すでに存在していたらエラーとする．
;; 以下のように修正する．

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
             (if (assoc next-inst labels)
                 (error "The same label name used --- ASSEMBLE" 
                        next-inst)
                 (receive insts
                          (cons (make-label-entry next-inst
                                                  insts)
                                labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

;;上のプログラムを実行してみると，
;;The same label name used --- ASSEMBLE here
;; 5.9

;;make-assign (p.313)をみると，演算子なしの代入でmake-primitive-expを使い，
;;演算子があるときはmake-operation-expを呼ぶ．このmake-operation-exp(p.316)
;;はまたmake-primitive-expを使っている．

;;make-primitive-expでlabelが見つかるが，演算から呼ばれたときはlabelは使えない
;;ようにすればよい．



(define ex5.9machine
 (make-machine
 '()
 (list (list '= =))
 '(start
   (test (op =) (label start) (label start)))))



;;次のように修正
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels '#t))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (make-primitive-exp exp machine labels use)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (if use
           (let ((insts
                  (lookup-label labels
                                (label-exp-label exp))))
             (lambda () insts))
           (error "Label used in operation --- ASSEMBLE" exp)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels '()))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

;;これで上のプログラムを実行すると
;;Label used in operation --- ASSEMBLE (label start)


;;5.11

;;a.

;;afterfib-n-2
;;の下の2行
;;(assign n (reg val))
;;(restore val)
;;to
;;(restore n)
;;change
;;Fib(n-1) Fib(n-2)は反対のレジスタに入るが結果は同じになる. 

;; 5.12

;;make-new-machine の変数に db (database) を追加

;;dbを読むインターフェース手続き
(define (get-db machine)
  (machine 'get-db))

;;;dbを初期化する手続き make-new-machineの内部
(define (init-db)
  (set! db (map (lambda (x) (list x))
                (list 'assign 'test 'branch 'goto 'save 'restore 'perform
                      'label-reg 'save-reg 'restore-reg 'source)))
  'done)

;;二つのメッセージを追加
((eq? message 'init-db) (init-db))
((eq? message 'get-db) db)

;;初期化を起動
(init-db)

;;dbに新規追加する手続き
(define (insert! value record)
  (if (not (member value (cdr record)))
      (set-cdr! record (cons value (cdr record)))))

;;make-assocの中
(insert! inst (assoc 'assign (machine 'get-db)))
(let ((reg (assign-reg-name inst))
      (val (assign-value-exp inst))
      (record (assoc 'source (machine 'get-db))))
  (if (assoc reg (cdr record))
      (insert! val (assoc reg (cdr record)))
      (set-cdr! record (cons (list reg val) (cdr record)))))

;;make-test
(insert! inst (assoc 'test (machine 'get-db)))
;;make-branch
(insert! inst (assoc 'branch (machine 'get-db)))
;;make-goto
(insert! inst (assoc 'goto (machine 'get-db)))

(insert! (register-exp-reg dest)
                     (assoc 'label-reg (machine 'get-db)))
;;make-save
(insert! inst (assoc 'save (machine 'get-db)))
(insert! (stack-inst-reg-name inst)
         (assoc 'save-reg (machine 'get-db)))
;;make-restore
(insert! inst (assoc 'restore (machine 'get-db)))
(insert! (stack-inst-reg-name inst)
         (assoc 'restore-reg (machine 'get-db)))
;;make-perform
(insert! inst (assoc 'perform (machine 'get-db)))

;;を追加する

;;fibonacci-machineを定義し (get-db 'fibonacci-machine)を実行すると

((assign
 (assign val (reg n))
 (assign val (op +) (reg val) (reg n))
 (assign n (reg val))
 (assign continue (label afterfib-n-2))
 (assign n (op -) (reg n) (const 2))
 (assign n (op -) (reg n) (const 1))
 (assign continue (label afterfib-n-1))
 (assign continue (label fib-done)))
 (test
 (test (op <) (reg n) (const 2)))
 (branch
 (branch (label immediate-answer)))
 (goto
 (goto (reg continue))
 (goto (label fib-loop)))
 (save
 (save val)
 (save n)
 (save continue))
 (restore
 (restore val)
 (restore continue)
 (restore n))
 (perform)
 (label-reg continue)
 (save-reg val n continue)
 (restore-reg val continue n)
 (source
 (val ((reg n)) ((op +) (reg val) (reg n)))
 (n ((reg val)) ((op -) (reg n) (const 2)) ((op -) (reg n) (const 1)))
 (continue ((label afterfib-n-2)) ((label afterfib-n-1)) ((label fib-done)))))
;;が得られる. 


;;5.14

;;まず階乗計算機を定義し

(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '- -) (list '* *))
   '((perform (op initialize-stack))
     (assign continue (label fact-done))
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))
   (goto (reg continue))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 fact-done
   (perform (op print-stack-statistics)))))

;;スタックをテストするプログラムを書き
(define (stack-test n)
(newline)(display "n = ") (display n)
(set-register-contents! fact-machine 'n n)
(start fact-machine)
(newline)(display (get-register-contents fact-machine 'val)))


(stack-test 1) 
(stack-test 2)
(stack-test 3)
(stack-test 4)
(stack-test 5)
(stack-test 6)
(stack-test 7)
(stack-test 8)
(stack-test 9)
(stack-test 10)

;;でテストする．実行結果

;;n = 1
(total-pushes = 0 maximum-depth = 0)
;;1
;;n = 2
(total-pushes = 2 maximum-depth = 2)
;;2
;;n = 3
(total-pushes = 4 maximum-depth = 4)
;;6
;;n = 4
;;(total-pushes = 6 maximum-depth = 6)
;;24
;;n = 5
(total-pushes = 8 maximum-depth = 8)
;;120
;;n = 6
;;(total-pushes = 10 maximum-depth = 10)
;;720
;;n = 7
(total-pushes = 12 maximum-depth = 12)
;;5040
;;n = 8
;;(total-pushes = 14 maximum-depth = 14)
;;40320
;;n = 9
;;(total-pushes = 16 maximum-depth = 16)
;;362880
;;n = 10
(total-pushes = 18 maximum-depth = 18)
;;3628800 -- done
;;Unspecified return value

;; 5.15

;;make-new-machineの変数にinst-countを用意する

;;(execute)の中でinst-countを増やす

 (set! inst-count (+ inst-count 1))

;;インターフェース
(define (reset-inst-count machine)
  (machine 'reset-inst-count))

(define (get-inst-count machine)
  (machine 'get-inst-count))

;;メッセージ処理

 ((eq? message 'get-inst-count) inst-count)
 ((eq? message 'reset-inst-count) (set! inst-count 0) 'ok)


;;fibonacci-machineを実行してみる


;Value: fibonacci-machine

;;1 ]=> (set-register-contents! fibonacci-machine 'n 10)

;Value: done

;;1 ]=> (start fibonacci-machine)

;Value: done

;;;1 ]=> (get-inst-count fibonacci-machine)

;Value: 2029

;;1 ]=> (get-register-contents fibonacci-machine 'val)

;Value: 55


;; 5.16

;;make-new-machineの中に

(define trace-flag '())

;;を追加し

;;executeの中でトレースする

  (if trace-flag
      (begin (newline) (display (caar insts))))

 (perform (op trace-on))  でトレース開始
 (perform (op trace-off)) でトレース終了


(define fibonacci-machine
  (make-machine
   '(continue n val)
   (list (list '< <) (list '- -) (list '+ +))
;;         (list 'trace-on trace-on) (list 'trace-off trace-off)) ;;opを追加
;; 演算子のリストにtrace-on, trace-offはいらない, make-new-machineで定義済み

   '(
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (goto (label fib-loop))
 afterfib-n-1
   (restore n)
   (perform (op trace-on))          ;; trace-on
   (restore continue)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)
   (goto (label fib-loop))
 afterfib-n-2
   (assign n (reg val))
   (restore val)
   (perform (op trace-off))
   (restore continue)
   (assign val (op +) (reg val) (reg n)) 
   (goto (reg continue))
 immediate-answer
   (assign val (reg n))
   (goto (reg continue))
 fib-done)))

(set-register-contents! fibonacci-machine 'n 2)
(start fibonacci-machine)
(get-register-contents fibonacci-machine 'val)

n=2で実行してみる

(assign continue (label fib-done))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(save continue)
(assign continue (label afterfib-n-1))
(save n)
(assign n (op -) (reg n) (const 1))
(goto (label fib-loop))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n))
(goto (reg continue))
(restore n)
(restore continue)
(assign n (op -) (reg n) (const 2))
(save continue)
(assign continue (label afterfib-n-2))
(save val)
(goto (label fib-loop))
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n))
(goto (reg continue))
(assign n (reg val))
(restore val)
(restore continue)
(assign val (op +) (reg val) (reg n))
(goto (reg continue)) -- done
;Value: 1


問題 5.17

ラベルが現れたらextract-labelsでinstの前に ((label )) を追加しておく．

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
             (let ((insts (cons (list (list 'label next-inst)) insts)));;ex5.17
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels)))
               (receive (cons (make-instruction next-inst)
                              insts)
                        labels)))))))

make-execution-procedure では(label )が来たら

        ((eq? (car inst) 'label) 
         (lambda () (monitor machine inst) (advance-pc pc)))

と実行手続きを返す．

一方(execute)は

(define (monitor machine inst)   ;;ex5.12
 (cond ((eq? (car inst) 'label)(newline)(display (cadr inst)) (display ':))
       (else ...

(label )ならdatabaseの処理にいかず，:を印字する．


fibonacci-machine で n=2 のアセンブリとトレースは以下のようになる

insts

(((assign continue (label fib-done))) ((label fib-loop))
 ((test (op <) (reg n) (const 2))) ((branch (label immediate-answer)))
 ((save continue)) ((assign continue (label afterfib-n-1))) ((save n))
 ((assign n (op -) (reg n) (const 1))) ((goto (label fib-loop)))
 ((label afterfib-n-1)) ((restore n)) ((restore continue))
 ((assign n (op -) (reg n) (const 2))) ((save continue))
 ((assign continue (label afterfib-n-2))) ((save val))
 ((goto (label fib-loop))) ((label afterfib-n-2)) ((assign n (reg val)))
 ((restore val)) ((restore continue)) ((assign val (op +) (reg val) (reg n)))
 ((goto (reg continue))) ((label immediate-answer)) ((assign val (reg n)))
 ((goto (reg continue))) ((label fib-done)))

labels

((fib-loop ((label fib-loop)) ((test (op <) (reg n) (const 2)))
 ((branch (label immediate-answer))) ((save continue))
 ((assign continue (label afterfib-n-1))) ((save n))
 ((assign n (op -) (reg n) (const 1))) ((goto (label fib-loop)))
 ((label afterfib-n-1)) ((restore n)) ((restore continue))
 ((assign n (op -) (reg n) (const 2))) ((save continue))
 ((assign continue (label afterfib-n-2))) ((save val))
 ((goto (label fib-loop))) ((label afterfib-n-2)) ((assign n (reg val)))
 ((restore val)) ((restore continue)) ((assign val (op +) (reg val) (reg n)))
 ((goto (reg continue))) ((label immediate-answer)) ((assign val (reg n)))
 ((goto (reg continue))) ((label fib-done)))

 (afterfib-n-1 ((label afterfib-n-1)) ((restore n)) ((restore continue))
 ((assign n (op -) (reg n) (const 2))) ((save continue))
 ((assign continue (label afterfib-n-2))) ((save val))
 ((goto (label fib-loop))) ((label afterfib-n-2)) ((assign n (reg val)))
 ((restore val)) ((restore continue)) ((assign val (op +) (reg val) (reg n)))
 ((goto (reg continue))) ((label immediate-answer)) ((assign val (reg n)))
 ((goto (reg continue))) ((label fib-done)))

 (afterfib-n-2 ((label afterfib-n-2)) ((assign n (reg val))) ((restore val))
 ((restore continue)) ((assign val (op +) (reg val) (reg n)))
 ((goto (reg continue))) ((label immediate-answer)) ((assign val (reg n)))
 ((goto (reg continue))) ((label fib-done)))

 (immediate-answer ((label immediate-answer)) ((assign val (reg n)))
 ((goto (reg continue))) ((label fib-done)))

 (fib-done ((label fib-done))))

実行トレース
(assign continue (label fib-done))
fib-loop:
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
(save continue)
(assign continue (label afterfib-n-1))
(save n)
(assign n (op -) (reg n) (const 1))
(goto (label fib-loop))
fib-loop:
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
immediate-answer:
(assign val (reg n))
(goto (reg continue))
afterfib-n-1:
(restore n)
(restore continue)
(assign n (op -) (reg n) (const 2))
(save continue)
(assign continue (label afterfib-n-2))
(save val)
(goto (label fib-loop))
fib-loop:
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
immediate-answer:
(assign val (reg n))
(goto (reg continue))
afterfib-n-2:
(assign n (reg val))
(restore val)
(restore continue)
(assign val (op +) (reg val) (reg n))
(goto (reg continue))
fib-done: -- done
;Value: 1
問題 5.18

;レジスタの内容を見る/止める ;ex5.18
;(set-register-trace!  )
;(reset-register-trace!  )

make-registerの変数にtrace-flagを追加する

(let ((contents '*unassigned*) (trace-flag '()))  ;;ex5.18

メッセージsetを修正する

      ((eq? message 'set)
       (lambda (value) 
         (if trace-flag
           (begin (newline) (display "register name:") (display name)
                  (newline) (display "old content:") (display contents)
                  (newline) (display "new content:") (display value)))
         (set! contents value)))

メッセージtrace-on trace-offを追加する

      ((eq? message 'trace-on) (set! trace-flag #t))
      ((eq? message 'trace-off) (set! trace-falg '()))

インターフェースを追加する

(define (set-register-trace! machine register-name)
  (set-trace (get-register machine register-name)))

(define (reset-register-trace! machine register-name)
  (reset-trace (get-register machine register-name)))


問題 5.19

;ブレークポイント ;ex5.19
;(set-breakpoint   )    ;設定
;(cancel-breakpoint   ) ;削除
;(cancel-all-breakpoints )    ;全体削除
;(proceed-machine )           ;再起動

ブレークポイントの命令は
(assign  ...)
の代わりに
((assign  )  ...)
のように作る. 
(execute)で命令の先頭がpairなら止まる. 

インターフェース

(define (set-breakpoint machine label n) ;ex5.19
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n) ;ex5.19
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (proceed-machine machine) ;ex5.19
  (machine 'proceed-machine))

ラベルを探すため make-new-machineにthe-labelsを変数を追加する
        (the-labels '()) ;ex5.19

executeでブレークポイントをさがす. 
              (if (pair? (caaar insts)) (cdaaar insts) ;ex5.19

make-new-machineにブレークポイント設定削除の手続きをおく. 

      (define (set-breakpoint label n)  ;ex5.19 設定
        (let ((insts (lookup-label the-labels label))
          (breakpoint-id (list 'breakpoint label n)))
        (define (insert-breakpoint insts n) ;設定の下請け ループ
          (cond ((null? insts) (error "breakpoint too far -- SETBREAKPOINT" n))
                ((eq? (caaar insts) 'label) (insert-breakpoint (cdr insts) n))
                ((> n 0) (insert-breakpoint (cdr insts) (- n 1)))
                (else (set-car! (caar insts) 
                                (cons (caaar insts) breakpoint-id)))))
        (insert-breakpoint insts n)
       'done))

      (define (cancel-breakpoint label n)   ;ex5.19 削除
        (let ((insts (lookup-label the-labels label)))
       (define (remove-breakpoint insts n)  ;削除の下請け ループ
         (cond ((null? insts) 
                (error "breakpoint too far -- CANCELBREAKPOINT" n))
               ((eq? (caaar insts) 'label) (remove-breakpoint (cdr insts) n))
               ((> n 0) (remove-breakpoint (cdr insts) (- n 1)))
               ((pair? (caaar insts))
                (set-car! (caar insts) (caaaar insts)))))
       (remove-breakpoint insts n)
       'done))

      (define (dispatch message)
(define (reset insts)
  (if (pair? insts)
      (if (pair? (caaar insts))
                 (set-car! (caar insts) (caaaar insts)))
                 (reset (cdr insts))))

メッセージ処理の追加
              ((eq? message 'set-breakpoint)            ;ex5.19
               (lambda (label n) (set-breakpoint label n)))
              ((eq? message 'cancel-breakpoint)         ;ex5.19
               (lambda (label n) (cancel-breakpoint label n)))
              ((eq? message 'cancel-all-breakpoints)    ;ex5.19
               (reset the-instruction-sequence)
               'done)

the-labelsを設定するため, assembleはinstsの他にlabelsもドット対にして返す. 
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      (cons insts labels)))) ;ex5.19

install-instruction-sequenceでもどってきたthe-labelsを設定する

              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence (car seq))

                             (set! the-labels (cdr seq))))  ;ex5.19










問題 5.21
;a
(define count-leaves
  (make-machine
    '(continue tree val)
    (list (list 'null? null?) (list 'pair? pair?) (list '+ +)
          (list 'car car) (list 'cdr cdr))
    '((assign continue (label done))
count-leaves
      (test (op null?) (reg tree))
      (branch (label null))
      (test (op pair?) (reg tree))
      (branch (label pair))
      (assign val (const 1))
      (goto (reg continue))
null
      (assign val (const 0))
      (goto (reg continue))
pair
      (save continue)
      (assign continue (label after-1))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label count-leaves))
after-1
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (assign continue (label after-2))
      (save val)
      (goto (label count-leaves))
after-2
      (assign tree (reg val))
      (restore val)
      (assign val (op +) (reg val) (reg tree))
      (restore continue)
      (goto (reg continue))
done)))

;b 
(define count-leaves
  (make-machine
    '(continue tree n val)
    (list (list 'null? null?) (list 'pair? pair?) (list '+ +)
          (list 'car car) (list 'cdr cdr))
    '((assign n (const 0))
      (assign continue (label done))
count-iter
      (test (op null?) (reg tree))
      (branch (label null))
      (test (op pair?) (reg tree))
      (branch (label pair))
      (assign val (op +) (reg n) (const 1))
      (goto (reg continue))
pair 
      (save continue)      
      (save tree)
      (assign tree (op car) (reg tree))
      (assign continue (label after1))
      (goto (label count-iter))
after1
      (assign n (reg val))
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (assign continue (label after2))
      (goto (label count-iter))
after2
      (restore continue)
      (goto (reg continue))
null
      (assign val (reg n))
      (goto (reg continue))
done)))
問題 5.22
;a
(define append-machine
 (make-machine
 '(continue x y val)
 (list (list 'null? null?) (list 'car car) (list 'cdr cdr)
       (list 'cons cons))
 '((assign continue (label done))
append
   (test (op null?) (reg x))
   (branch (label null))
   (save continue)
   (save x)
   (assign x (op cdr) (reg x))
   (assign continue (label after-append))
   (goto (label append))
after-append
   (restore x)
   (assign x (op car) (reg x))
   (assign val (op cons) (reg x) (reg val))
   (restore continue)
   (goto (reg continue))
null
   (assign val (reg y))
   (goto (reg continue))
done)))

;b
(define append!-machine
 (make-machine
 '(x y z)
 (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
 '((save x)
loop
   (assign z (op cdr) (reg x))
   (test (op null?) (reg z))
   (branch (label null))
   (assign x (op cdr) (reg x))
   (goto (label loop))
null
   (perform (op set-cdr!) (reg x) (reg y))
   (restore x))))




積極制御評価器
;; eceval

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
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
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define apply-in-underlying-scheme apply)

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '))
      (display object)))
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
        ))
(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (get-global-environment)
  the-global-environment)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (no-more-exps? seq) (null? seq))

(define eceval-operations
  (list 
(list 'adjoin-arg adjoin-arg)
(list 'announce-output announce-output)
(list 'application? application?)
(list 'apply-primitive-procedure apply-primitive-procedure)
(list 'assignment-value assignment-value)
(list 'assignment-variable assignment-variable)
(list 'assignment? assignment?)
(list 'begin-actions begin-actions)
(list 'begin? begin?)
(list 'compiled-procedure? compiled-procedure?)
(list 'compound-procedure? compound-procedure?)
(list 'define-variable! define-variable!)
(list 'definition-value definition-value)
(list 'definition-variable definition-variable)
(list 'definition? definition?)
(list 'empty-arglist empty-arglist)
(list 'extend-environment extend-environment)
(list 'first-exp first-exp)
(list 'first-operand first-operand)
(list 'get-global-environment get-global-environment)
(list 'if-alternative if-alternative)
(list 'if-consequent if-consequent)
(list 'if-predicate if-predicate)
(list 'if? if?)
;(list 'initialize-stack initialize-stack)
(list 'lambda-body lambda-body)
(list 'lambda-parameters lambda-parameters)
(list 'lambda? lambda?)
(list 'last-exp? last-exp?)
(list 'last-operand? last-operand?)
(list 'lookup-variable-value lookup-variable-value)
(list 'make-procedure make-procedure)
(list 'no-more-exps? no-more-exps?)
(list 'no-operands? no-operands?)
(list 'operands operands)
(list 'operator operator)
(list 'primitive-procedure? primitive-procedure?)
;(list 'print-stack-statistics print-stack-statistics)
(list 'procedure-body procedure-body)
(list 'procedure-environment procedure-environment)
(list 'procedure-parameters procedure-parameters)
(list 'prompt-for-input prompt-for-input)
(list 'quoted? quoted?)
(list 'read read)
(list 'rest-exps rest-exps)
(list 'rest-operands rest-operands)
(list 'self-evaluating? self-evaluating?)
(list 'set-variable-value! set-variable-value!)
(list 'text-of-quotation text-of-quotation)
(list 'true? true?)
(list 'user-print user-print)
(list 'variable? variable?)
))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
  eceval-operations
  '(
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))

print-result
  (perform (op print-stack-statistics));;スタック統計量出力
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))

ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))

ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

;; 末尾再帰 ev-sequence
ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;末尾再帰なし ev-sequence
;;ev-sequence
;;  (test (op no-more-exps?) (reg unev))
;;  (branch (label ev-sequence-end))
;;  (assign exp (op first-exp) (reg unev))
;;  (save unev)
;;  (save env)
;;  (assign continue (label ev-sequence-continue))
;;  (goto (label eval-dispatch))
;;ev-sequence-continue
;;  (restore env)
;;  (restore unev)
;;  (assign unev (op rest-exps) (reg unev))
;;  (goto (label ev-sequence))
;;ev-sequence-end
;;  (restore continue)
;;  (goto (reg continue))

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

   )))

(define the-global-environment (setup-environment))

(start eceval)
問題 5.23
;; ここまでの定義ではcond, letはunbound variableのエラーになる．

;condの実装
; 221ページのcondの実装を借りてくる．

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (sequence->exp exps)  ;新しく定義
  (cons 'begin exps))

(define (make-if predicate consequence  alternative) ;新しく定義
   (list 'if predicate consequence alternative))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;eceval-operationsに以下を追加

(list 'cond? cond?)         ;;ex5.23
(list 'cond->if cond->if)   ;;ex5.23

;eval-dispatchに以下を追加

  (test (op cond?) (reg exp))     ;;ex5.23
  (branch (label ev-cond))

;以下を追加
ev-cond            ;; ex5.23
  (assign exp (op cond->if) (reg exp))
  (goto (label ev-if))

;letの実装
;問題4.6を借りる

;以下を追加
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((bindings (cadr exp)) (body (cddr exp)))
   (cons (make-lambda (map car bindings) body)
     (map cadr bindings))))

;eceval-operations に以下を追加

(list 'let? let)            ;;ex5.23
(list 'let->combination let->combination)  ;;ex5.23

;eval-dispatchに以下を追加
 
 (test (op let?) (reg exp))      ;;ex5.23
  (branch (label ev-let))

問題 5.24
ev-cond              ;;ex5.24
  (save continue)
  (assign unev (op cond-clauses) (reg exp))
ev-cond-loop
  (test (op null?) (reg unev))
  (branch (label ev-cond-null))
  (assign exp (op first-exp) (reg unev))
  (assign unev (op rest-exps) (reg unev))
  (test (op cond-else-clause?) (reg exp))
  (branch (label ev-cond-else-clause))
  (save exp)
  (save unev)
  (save env)
  (assign continue (label ev-cond-decide))
  (assign exp (op cond-predicate) (reg exp))
  (goto (label eval-dispatch))
ev-cond-decide
  (restore env)
  (restore unev)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-cond-consequent))
  (goto (label ev-cond-loop))
ev-cond-consequent
  (assign unev (op cond-actions) (reg exp))
  (goto (label ev-sequence))
ev-cond-else-clause
  (test (op null?) (reg unev))
  (branch (label ev-cond-else-consequent))
  (assign val (const else-clause-isnt-last))
  (goto (label signal-error))
ev-cond-else-consequent
  (assign unev (op cond-actions) (reg exp))
  (goto (label ev-sequence))
ev-cond-null
  (assign val (const ()))
  (restore continue)
  (goto (reg continue))
問題 5.25
;; ev-applicationの変更

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-app-did-operator))
  (goto (label actual-value))
ev-app-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op primitive-procedure?) (reg proc))
  (branch (label list-of-arg-values))
  (test (op compound-procedure?) (reg proc))
  (branch (label list-of-delayed-args))
  (goto (label unknown-procedure-type))
list-of-arg-values
  (test (op no-operands?) (reg unev))
  (branch (label primitive-apply))
  (save proc)
arg-value-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label last-arg-value))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label actual-value))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label arg-value-loop))
last-arg-value
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label actual-value))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc) (reg argl))
  (restore continue)
  (goto (reg continue))
list-of-delayed-args
  (test (op no-operands?) (reg unev))
  (branch (label compound-apply))
  (assign exp (op first-operand) (reg unev))
  (assign val (op delay-it) (reg exp) (reg env))
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label list-of-delayed-args))
compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

actual-value
  (save continue)
  (assign continue (label eval-end))
  (goto (label eval-dispatch))
eval-end
  (assign exp (reg val))
  (restore continue)
  (goto (label force-it))

force-it
  (test (op thunk?) (reg exp))
  (branch (label eval-thunk))
  (assign val (reg exp))
  (goto (reg continue))
eval-thunk
  (assign env (op thunk-env) (reg exp))
  (assign exp (op thunk-exp) (reg exp))
  (goto (label actual-value))

;; ev-ifも修正する

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label actual-value))

;; 以下は基本命令として定義

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

;; eceval-operationsに追加する

(list 'thunk? thunk?)
(list 'thunk-exp thunk-exp)
(list 'thunk-env thunk-env)
(list 'delay-it delay-it)

;; 実行してみる. 
;;;;; EC-Eval input:
;;(define (try a b)
;;  (if (= a 0) 1 b))
;;
;;(total-pushes = 3 maximum-depth = 3)
;;;;; EC-Eval value:
;;ok
;;
;;;;; EC-Eval input:
;;(try 0 (/ 1 0))
;;
;;(total-pushes = 20 maximum-depth = 10)
;;;;; EC-Eval value:
;;1
;;

問題 5.26
;;; EC-Eval input:
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 1)

(total-pushes = 64 maximum-depth = 10)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 99 maximum-depth = 10)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 134 maximum-depth = 10)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 169 maximum-depth = 10)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 204 maximum-depth = 10)
;;; EC-Eval value:
120

 n  total  max
 1    64   10
 2    99   10
 3   134   10
 4   169   10
 5   204   10
問題 5.27
;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 1)

(total-pushes = 16 maximum-depth = 8)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 48 maximum-depth = 13)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 80 maximum-depth = 18)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 112 maximum-depth = 23)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 144 maximum-depth = 28)
;;; EC-Eval value:
120

 n  total  max
 1    16    8
 2    48   13
 3    89   18
 4   112   23
 5   144   28
問題 5.28
;;; EC-Eval input:
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 1)

(total-pushes = 70 maximum-depth = 17)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 107 maximum-depth = 20)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 144 maximum-depth = 23)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 181 maximum-depth = 26)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 218 maximum-depth = 29)
;;; EC-Eval value:
120

;;; EC-Eval input:
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 1)

(total-pushes = 18 maximum-depth = 11)
;;; EC-Eval value:
1

;;; EC-Eval input:
(factorial 2)

(total-pushes = 52 maximum-depth = 19)
;;; EC-Eval value:
2

;;; EC-Eval input:
(factorial 3)

(total-pushes = 86 maximum-depth = 27)
;;; EC-Eval value:
6

;;; EC-Eval input:
(factorial 4)

(total-pushes = 120 maximum-depth = 35)
;;; EC-Eval value:
24

;;; EC-Eval input:
(factorial 5)

(total-pushes = 154 maximum-depth = 43)
;;; EC-Eval value:
120

反復版            再帰版
 n  total  max    n  total  max
 1    70   17     1    18   11
 2   107   20     2    52   19
 3   144   23     3    86   27
 4   181   26     4   120   35
 5   218   29     5   154   43
問題 5.29
;;; EC-Eval input:
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 2)

(total-pushes = 72 maximum-depth = 13)
;;; EC-Eval value:
1

;;; EC-Eval input:
(fib 3)

(total-pushes = 128 maximum-depth = 18)
;;; EC-Eval value:
2

;;; EC-Eval input:
(fib 4)

(total-pushes = 240 maximum-depth = 23)
;;; EC-Eval value:
3

;;; EC-Eval input:
(fib 5)

(total-pushes = 408 maximum-depth = 28)
;;; EC-Eval value:
5

;;; EC-Eval input:
(fib 6)

(total-pushes = 688 maximum-depth = 33)
;;; EC-Eval value:
8

;;; EC-Eval input:
(fib 7)

(total-pushes = 1136 maximum-depth = 38)
;;; EC-Eval value:
13

;;; EC-Eval input:
(fib 8)

(total-pushes = 1864 maximum-depth = 43)
;;; EC-Eval value:
21

;;; EC-Eval input:
(fib 9)

(total-pushes = 3040 maximum-depth = 48)
;;; EC-Eval value:
34

;;; EC-Eval input:
(fib 10)

(total-pushes = 4944 maximum-depth = 53)
;;; EC-Eval value:
55

これより
 n fib(n) total  max
 2     1    72   13
 3     2   128   18
 4     3   240   23
 5     5   408   28
 6     8   688   33
 7    13  1136   38
 8    21  1864   43
 9    34  3040   48
10    55  4944   53

k=S(4)-S(3)-S(2)=40

S(n)=a Fib(n+1) + b  と書けるとすると
S(n)=S(n-1) + S(n-2) + k
    =a Fib(n) + b + a Fib(n-1) + b + k
    =a Fib(n+1) + b + (b + k) 
∴ b = -k = -40
   a = 56



翻訳系
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


(define (cond? exp) (tagged-list? exp 'cond))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assignment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                    (number->string (new-label-number)))))

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage))
            (a-code
             (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

(define (compile-sequence seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
       (compile (first-exp seq) target 'next)
       (compile-sequence (rest-exps seq) target linkage))))

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry))
       after-lambda))))

(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return))))

(define (compile-application exp target linkage)
  (let ((proc-code (compile (operator exp) 'proc 'next))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))
(define all-regs '(env proc val argl continue))
(define (compile-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

;;命令列の組合せ

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

翻訳系と使う積極制御評価器
;; eceval compile-and-run

(define (compile-and-run expression) ;; ch5.5.7
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))

(load "compile2.sch")

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))
(define (variable? exp) (symbol? exp))
(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
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
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define apply-in-underlying-scheme apply)

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))
;;(define (user-print object)
;;  (if (compound-procedure? object)
;;      (display (list 'compound-procedure
;;                     (procedure-parameters object)
;;                     (procedure-body object)
;;                     '))
;;      (display object)))
;;脚注50
(define (user-print object) 
  (cond ((compound-procedure? object)
         (display (list 'compound-procedure
                         (procedure-parameters object)
                         (procedure-body object)
                         ')))
        ((compiled-procedure? object)
         (display '))
        (else (display object))))

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
        (list 'compile-and-run compile-and-run)
   ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))
(define (get-global-environment)
  the-global-environment)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (no-more-exps? seq) (null? seq))

;;脚注38
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

(define eceval-operations
  (list 
(list 'adjoin-arg adjoin-arg)
(list 'announce-output announce-output)
(list 'application? application?)
(list 'apply-primitive-procedure apply-primitive-procedure)
(list 'assignment-value assignment-value)
(list 'assignment-variable assignment-variable)
(list 'assignment? assignment?)
(list 'begin-actions begin-actions)
(list 'begin? begin?)
(list 'compile-and-run compile-and-run)
(list 'compiled-procedure-entry compiled-procedure-entry)
(list 'compiled-procedure? compiled-procedure?)
(list 'compound-procedure? compound-procedure?)
(list 'define-variable! define-variable!)
(list 'definition-value definition-value)
(list 'definition-variable definition-variable)
(list 'definition? definition?)
(list 'empty-arglist empty-arglist)
(list 'extend-environment extend-environment)
(list 'first-exp first-exp)
(list 'first-operand first-operand)
(list 'get-global-environment get-global-environment)
(list 'if-alternative if-alternative)
(list 'if-consequent if-consequent)
(list 'if-predicate if-predicate)
(list 'if? if?)
;(list 'initialize-stack initialize-stack)
(list 'lambda-body lambda-body)
(list 'lambda-parameters lambda-parameters)
(list 'lambda? lambda?)
(list 'last-exp? last-exp?)
(list 'last-operand? last-operand?)
(list 'lookup-variable-value lookup-variable-value)
(list 'make-procedure make-procedure)
(list 'no-more-exps? no-more-exps?)
(list 'no-operands? no-operands?)
(list 'operands operands)
(list 'operator operator)
(list 'primitive-procedure? primitive-procedure?)
;(list 'print-stack-statistics print-stack-statistics)
(list 'procedure-body procedure-body)
(list 'procedure-environment procedure-environment)
(list 'procedure-parameters procedure-parameters)
(list 'prompt-for-input prompt-for-input)
(list 'quoted? quoted?)
(list 'read read)
(list 'rest-exps rest-exps)
(list 'rest-operands rest-operands)
(list 'self-evaluating? self-evaluating?)
(list 'set-variable-value! set-variable-value!)
(list 'text-of-quotation text-of-quotation)
(list 'true? true?)
(list 'user-print user-print)
(list 'variable? variable?)

(list 'compiled-procedure-env compiled-procedure-env)
(list 'cons cons)
(list 'false? false?)
(list 'list list)
(list 'make-compiled-procedure make-compiled-procedure)
))

(define eceval
  (make-machine
   '(exp env val proc argl continue unev compapp)
   eceval-operations
  '(
  (assign compapp (label compound-apply))
  (branch (label external-entry))
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))

external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))
;   (goto (label eval-dispatch))

print-result
  (perform (op print-stack-statistics));;スタック統計量出力
  (perform
;;脚注38
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-compile-and-run
  (perform (op compile-and-run) (reg exp))
  (goto (reg continue))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))

ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))

ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))

ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)

ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))

apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (test (op compiled-procedure?) (reg proc))  
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))

compiled-apply
  (restore continue)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

;apply-dispatch
;  (test (op primitive-procedure?) (reg proc))
;  (branch (label primitive-apply))
;  (test (op compound-procedure?) (reg proc))  
;  (branch (label compound-apply))
;  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

;; 末尾再帰 ev-sequence
ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;末尾再帰なし ev-sequence
;;ev-sequence
;;  (test (op no-more-exps?) (reg unev))
;;  (branch (label ev-sequence-end))
;;  (assign exp (op first-exp) (reg unev))
;;  (save unev)
;;  (save env)
;;  (assign continue (label ev-sequence-continue))
;;  (goto (label eval-dispatch))
;;ev-sequence-continue
;;  (restore env)
;;  (restore unev)
;;  (assign unev (op rest-exps) (reg unev))
;;  (goto (label ev-sequence))
;;ev-sequence-end
;;  (restore continue)
;;  (goto (reg continue))

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))
   )))

(define the-global-environment (setup-environment))

(define (start-eceval)  ;;脚注49
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))

(start-eceval)
問題 5.31
329ページのev-applicationをみて, 問題のレジスタ, env, proc, arglの退避回復
に注目すると

ev-application
  (save env)
  <演算子の評価>
ev-appl-did-operator
  (restore env)
  
  
  <被演算子がなければapply-dispatchへ>
  (save proc)
ev-appl-operand-loop
  (save argl)
  
  <最後の被演算子ならev-appl-last-argへ>
  (save env)
  <被演算子の評価>
ev-appl-accumulate-arg
  (restore env)
  (restore argl)
  (arglに評価済みの被演算子を追加>
  
ev-appl-last-arg
  <最後の被演算子の評価>
ev-appl-accum-last-arg
  (restore argl)
  
  (restore proc)
  

で問題文に書いてあるようになっている. 

これらのレジスタはどこでassignされるかをみると
env はapplyで定義された手続きを呼ぶとき, 手続きのenvに仮引数, 実引数の
ペアを追加してenvにおく. 

proc は組み合わせの評価で演算子を評価してprocにおく. 

argl は組み合わせの評価で被演算子をおく. 

(f 'x 'y) 演算子, 被演算子の評価に組み合わせがないので, save, restoreは不要. 

((f)) 'x 'y) 演算子の評価に組み合わせがあるが, procもarglもこれから使うので
save, restore不要

(f (g 'x) y) procの評価のあとyをarglにおき, そこで組み合わせ(g 'x)を評価する
ので(g 'x)の評価の前後でproc, arglのsave, restoreが必要

(f (g 'x) 'y) うえと同じ. 

問題 5.33
factorial-altを翻訳すると次のようになる. fig5.17と比べてみると, A, B, Cの部分は共通である. 図5.17ではAの次に (assign val (op lookup-variable-value) (const n) (reg env)) があってnの値をvalに置き, それをlistにしてarglに置き, Bの前にarglを退避する. Bが済むとarglを回復してfactorial(n-1)をconsしてarglを構成する. このfactorial-altではfactorialを先に計算するので, Aの後でenvを退避し, factorial の計算後, envを回復して, nの値をとり, arglにconsする. nの値をとりにいく前後で, arglを退避する必要はない. つまり一方はenvを退避, 回復し, 他方はarglを退避, 回復するだけの違いで, 計算効率 には差がない.
↑  (assign val (op make-compiled-procedure) (label entry2) (reg env))
│  (goto (label after-lambda1))
│entry2
│  (assign env (op compiled-procedure-env) (reg proc))
│  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
│  (save continue)
│  (save env)
│  (assign proc (op lookup-variable-value) (const =) (reg env))
│  (assign val (const 1))
│  (assign argl (op list) (reg val))
│  (assign val (op lookup-variable-value) (const n) (reg env))
│  (assign argl (op cons) (reg val) (reg argl))
│  (test (op primitive-procedure?) (reg proc))
│  (branch (label primitive-branch17))
│compiled-branch16
A   (assign continue (label after-call15))
│  (assign val (op compiled-procedure-entry) (reg proc))
│  (goto (reg val))
│primitive-branch17
│  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
│after-call15
│  (restore env)
│  (restore continue)
│  (test (op false?) (reg val))
│  (branch (label false-branch4))
│true-branch5
│  (assign val (const 1))
│  (goto (reg continue))
│false-branch4
│  (assign proc (op lookup-variable-value) (const *) (reg env))
│  (save continue)
↓  (save proc)
    (save env)
↑ (assign proc (op lookup-variable-value) (const factorial-alt) (reg env))
│  (save proc)
│  (assign proc (op lookup-variable-value) (const /-) (reg env))
│  (assign val (const 1))
│  (assign argl (op list) (reg val))
│  (assign val (op lookup-variable-value) (const n) (reg env))
│  (assign argl (op cons) (reg val) (reg argl))
│  (test (op primitive-procedure?) (reg proc))
│  (branch (label primitive-branch8))
│compiled-branch7
│  (assign continue (label after-call6))
│  (assign val (op compiled-procedure-entry) (reg proc))
│  (goto (reg val))
B primitive-branch8
│  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
│after-call6
│  (assign argl (op list) (reg val))
│  (restore proc)
│  (test (op primitive-procedure?) (reg proc))
│  (branch (label primitive-branch11))
│compiled-branch10
│  (assign continue (label after-call9))
│  (assign val (op compiled-procedure-entry) (reg proc))
│  (goto (reg val))
│primitive-branch11
│  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
↓after-call9
    (assign argl (op list) (reg val))
    (restore env)
    (assign val (op lookup-variable-value) (const n) (reg env))
↑  (assign argl (op cons) (reg val) (reg argl))
│  (restore proc)
│  (restore continue)
│  (test (op primitive-procedure?) (reg proc))
│  (branch (label primitive-branch14))
│compiled-branch13
│  (assign val (op compiled-procedure-entry) (reg proc))
C   (goto (reg val))
│primitive-branch14
│  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
│  (goto (reg continue))
│after-call12
│after-if3
│after-lambda1
│  (perform (op define-variable!) (const factorial-alt) (reg val) (reg env))
↓  (assign val (const ok))
問題 5.34
反復階乗手続きを翻訳すると次のようになる.
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))
entry7
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch9))
true-branch10
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch9
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const /+) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if8
after-lambda6
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call3
after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
ここでsaveとrestoreの関係を見ると
  ┌─    (save continue)
  │┌  (save env)
  ││  (> counter n)の計算
  ││after-call20
  │└  (restore env)
  └─  (restore continue)
      false-branch9
┌──  (save continue)
│┌─  (save proc)
││┌  (save env)
│││(+ counter 1)の計算
│││after-call11
││└  (restore env)
││┌  (save argl)
│││(* counter product)の計算
││└  (restore argl)
│└─  (restore proc)
└──  (restore continue)
      (iter counter product)の計算
iterの計算の時はstackを使っていない. 一方fig5.17でも同様の関係を書くと
    ┌─  (save continue)
    │┌  (save env)
    ││(= n 1)の計算
    ││after-call15
    │└  (restore env)
    └─  (restore continue)
        false-branch4
┌───  (save continue)
│┌──  (save proc)
││┌─  (save argl)
│││┌  (save proc)
││││(- n 1)の計算
││││after-call6
│││└  (restore proc)
│││  (factorial (- n 1))の計算
│││  after-call9
││└─  (restore argl)
│└──  (restore proc)
└───  (restore continue)
       (* (factorial (- n 1)) n)の計算
       after-call12
となり, factorialの計算中は3段のスタックがつまれていることが分る. 

問題 5.35
ex5.35
(define (f x)
  (+ x (g (+ x 2))))
を翻訳する

label-numberが図5.18とは違っているが, 同じプログラムである. label-numberを
図5.18と同様にするにはcompilerのlabel-counterの初期値を
(define label-counter 14) (345ページ脚注37)
と設定する. 

;;;; 翻訳された手続き(compiled-procedure 入り口 環境)をvalに作り, 最後へ飛ぶ
;;  (assign val (op make-compiled-procedure) (label entry2) (reg env))
;;  (goto (label after-lambda1))
;;;; 手続き f の入り口
;;entry2
;;;; 翻訳された手続きから環境部分を取り出し, 
;;  (assign env (op compiled-procedure-env) (reg proc))
;;;; パラメタと引数で環境を拡張し, 
;;  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))
;;;; 環境から + の手続きをprocへ取り出し, 
;;  (assign proc (op lookup-variable-value) (const /+) (reg env))
;;  (save continue)
;;  (save proc)
;;  (save env)
;;;; レジスタを退避した後, 環境から g の手続きをprocへ取り出し, 
;;  (assign proc (op lookup-variable-value) (const g) (reg env))
;;  (save proc)
;;;; レジスタを退避した後, 環境から + の手続きをprocへ取り出し,
;;  (assign proc (op lookup-variable-value) (const /+) (reg env))
;;;; 2をvalへ置き, 
;;  (assign val (const 2))
;;;; listにしてarglへ置き, 
;;  (assign argl (op list) (reg val))
;;;; 環境から x の値をvalへ取り出し, 
;;  (assign val (op lookup-variable-value) (const x) (reg env))
;;; arglへconsし, (argl には (x 2)が出来る.)
;;  (assign argl (op cons) (reg val) (reg argl))
;;  (test (op primitive-procedure?) (reg proc))
;;;; procにある手続きが基本手続きであれば, primitive-branch5へ
;;  (branch (label primitive-branch5))
;;compiled-branch4
;;;; 翻訳手続きの場合, 帰り番地を設定し, 手続きの入り口をvalへ置いて飛ぶ, 
;;  (assign continue (label after-call3))
;;  (assign val (op compiled-procedure-entry) (reg proc))
;;  (goto (reg val))
;;primitive-branch5
;;  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;after-call3
;;;; 手続きの値はvalにある. 
;;;; x + 2の値がvalにある. リストにしてarglへ置く. 
;;  (assign argl (op list) (reg val))
;;;; gをprodへ回復し, 
;;  (restore proc)
;;;; 基本手続きであれば, primitive-branch8へ
;;  (test (op primitive-procedure?) (reg proc))
;;  (branch (label primitive-branch8))
;;compiled-branch7
;;  (assign continue (label after-call6))
;;  (assign val (op compiled-procedure-entry) (reg proc))
;;  (goto (reg val))
;;primitive-branch8
;;  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;after-call6
;;  (assign argl (op list) (reg val))
;;  (restore env)
;;  (assign val (op lookup-variable-value) (const x) (reg env))
;;  (assign argl (op cons) (reg val) (reg argl))
;;  (restore proc)
;;  (restore continue)
;;  (test (op primitive-procedure?) (reg proc))
;;  (branch (label primitive-branch11))
;;compiled-branch10
;;  (assign val (op compiled-procedure-entry) (reg proc))
;;  (goto (reg val))
;;primitive-branch11
;;  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
;;  (goto (reg continue))
;;after-call9
;;after-lambda1
;;  (perform (op define-variable!) (const f) (reg val) (reg env))
;;  (assign val (const ok))










問題 5.46
1 ]=> (compile-and-go
'(define (fib n)
  (if (< n 2) 
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(fib 2)

(total-pushes = 17 maximum-depth = 5)
;;; EC-Eval value:
1

;;; EC-Eval input:
(fib 3)

(total-pushes = 27 maximum-depth = 8)
;;; EC-Eval value:
2

;;; EC-Eval input:
(fib 4)

(total-pushes = 47 maximum-depth = 11)
;;; EC-Eval value:
3

;;; EC-Eval input:
(fib 5)

(total-pushes = 77 maximum-depth = 14)
;;; EC-Eval value:
5

;;; EC-Eval input:
(fib 6)

(total-pushes = 127 maximum-depth = 17)
;;; EC-Eval value:
8

;;; EC-Eval input:
(fib 7)


(total-pushes = 207 maximum-depth = 20)
;;; EC-Eval value:
13

;;; EC-Eval input:
(fib 8)

(total-pushes = 337 maximum-depth = 23)
;;; EC-Eval value:
21

;;; EC-Eval input:
(fib 9)

(total-pushes = 547 maximum-depth = 26)
;;; EC-Eval value:
34

;;; EC-Eval input:
(fib 10)

(total-pushes = 887 maximum-depth = 29)
;;; EC-Eval value:
55

;;; EC-Eval input:

これより
 n fib(n) total  max
 2     1    17    5
 3     2    27    8
 4     3    47   11
 5     5    77   14
 6     8   127   17
 7    13   207   20
 8    21   337   23
 9    34   547   26
10    55   887   29
問題 5.47
ex5.47にあるread-eval-print-loopの直前の追加, 積極制御評価機械の記述(336ページ)
のレジスタのリストにcompappを追加する他, compile-proc-applを以下のように修正

<標的>, <接続>の各組に, 5行を挿入, compound手続きへ飛ぶ準備をする. 

(define (compile-proc-appl target linkage)
 (let ((compound-branch (make-label 'compound-branch)))    ;;新labelを用意
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (test (op compound-procedure?) (reg proc))    ;;以下5行を挿入 
             (branch (label ,compound-branch))             ;;
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val))
             ,compound-branch                              ;;
             (save continue)                               ;;
             (goto (reg compapp)))))                       ;;
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (test (op compound-procedure?) (reg proc))   ;;以下5行を挿入
              (branch (label ,compound-branch))            ;;
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,compound-branch                             ;;
              (save continue)                              ;;
              (goto (reg compapp))                         ;;
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          `((test (op compound-procedure?) (reg proc))     ;;以下5行を挿入
            (branch (label ,compound-branch))              ;;
            (assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val))
            ,compound-branch                               ;;
            (save continue)                                ;;
            (goto (reg compapp)))))                        ;;
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target)))))


実行例

1 ]=> (compile-and-go
  '(define (f x) (+ x (g (+ x 2)))))        ;;手続きfを翻訳する. 

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(define (g x) (* x x))                      ;;手続きgを定義する. 

(total-pushes = 3 maximum-depth = 3)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(f 4)                                        ;;手続きfを呼び出す. 

(total-pushes = 18 maximum-depth = 8)
;;; EC-Eval value:
40
問題 5.48
ecevalに以下のcompile-and-run手続きを追加, 
primitive-proceduresに
        (list 'compile-and-run compile-and-run)
の行を追加する. 

(define (compile-and-run expression) ;; ch5.5.7
  (let ((instructions
         (assemble (statements
                    (compile expression 'val 'return))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
実行例

;;; EC-Eval input:
(compile-and-run
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(factorial 5)

(total-pushes = 31 maximum-depth = 14)
;;; EC-Eval value:
120

;;; EC-Eval input:
(compile-and-run
 '(define (append x y)
    (if (null? x)
        y
        (cons (car x)
              (append (cdr x) y)))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(append '(a b c) '(d e f))

(total-pushes = 34 maximum-depth = 11)
;;; EC-Eval value:
(a b c d e f)

;;; EC-Eval input:
(compile-and-run
 '(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b)))))

(total-pushes = 0 maximum-depth = 0)
;;; EC-Eval value:
ok

;;; EC-Eval input:
(gcd 206 40)

(total-pushes = 30 maximum-depth = 5)
;;; EC-Eval value:
2




















