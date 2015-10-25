;;-4- 55

;; ;;; Query input:
;; (supervisor ?x (Bitdiddle Ben))
;;
;; ;;; Query results:
;; (supervisor (tweakit lem e) (bitdiddle ben))
;; (supervisor (fect cy d) (bitdiddle ben))
;; (supervisor (hacker alyssa p) (bitdiddle ben))
;;
;; ;;; Query input:
;; (job ?x (accounting . ?y))
;;
;; ;;; Query results:
;; (job (cratchet robert) (accounting scrivener))
;; (job (scrooge eben) (accounting chief accountant))
;;
;; ;;; Query input:
;; (address ?x (Slumerville . ?y))
;;
;; ;;; Query results:
;; (address (aull dewitt) (slumerville (onion square) 5))
;; (address (reasoner louis) (slumerville (pine tree road) 80))
;; (address (bitdiddle ben) (slumerville (ridge road) 10))
;;

;;4-56
;; ;;; Query input:
;; (supervisor ?x (Bitdiddle Ben))
;;
;; ;;; Query results:
;; (supervisor (tweakit lem e) (bitdiddle ben))
;; (supervisor (fect cy d) (bitdiddle ben))
;; (supervisor (hacker alyssa p) (bitdiddle ben))
;;
;; ;;; Query input:
;; (and (salary (Bitdiddle Ben) ?x)
;;      (salary ?y ?z)
;;      (lisp-value < ?z ?x))
;;
;; ;;; Query results:
;; (and (salary (bitdiddle ben) 60000) (salary (aull dewitt) 25000)
;;  (lisp-value < 25000 60000))
;; (and (salary (bitdiddle ben) 60000) (salary (cratchet robert) 18000)
;;  (lisp-value < 18000 60000))
;; (and (salary (bitdiddle ben) 60000) (salary (reasoner louis) 30000)
;;  (lisp-value < 30000 60000))
;; (and (salary (bitdiddle ben) 60000) (salary (tweakit lem e) 25000)
;;  (lisp-value < 25000 60000))
;; (and (salary (bitdiddle ben) 60000) (salary (fect cy d) 35000)
;;  (lisp-value < 35000 60000))
;; (and (salary (bitdiddle ben) 60000) (salary (hacker alyssa p) 40000)
;;  (lisp-value < 40000 60000))
;;
;; ;;; Query input:
;; (and (supervisor ?x ?y)
;;      (job ?y ?w)
;;      (not (job ?y (computer . ?z))))
;;
;; ;;; Query results:
;; (and (supervisor (aull dewitt) (warbucks oliver))
;;  (job (warbucks oliver) (administration big wheel))
;;  (not (job (warbucks oliver) (computer . ?z))))
;; (and (supervisor (cratchet robert) (scrooge eben))
;;  (job (scrooge eben) (accounting chief accountant))
;;  (not (job (scrooge eben) (computer . ?z))))
;; (and (supervisor (scrooge eben) (warbucks oliver))
;;  (job (warbucks oliver) (administration big wheel))
;;  (not (job (warbucks oliver) (computer . ?z))))
;; (and (supervisor (bitdiddle ben) (warbucks oliver))
;;  (job (warbucks oliver) (administration big wheel))
;;  (not (job (warbucks oliver) (computer . ?z))))


;;4-57
(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (administration secretary)
                     (administration big wheel)))

(assert! (rule (same ?x ?x)))

(assert! (rule (replace ?p1 ?p2)
               (and   (or (and (job ?p1 ?j) (job ?p2 ?j))
                          (and (job ?p1 ?j1) (job ?p2 ?j2)
                               (can-do-job ?j1 ?j2)))
                      (not (same ?p1 ?p2)))))

;; (not (same ?p1 ?p2))は条件の最後におく.

(replace ?x (fect cy d))

(assert! (rule (ex4.57.b ?p1 ?s1 ?p2 ?s2)
               (and (replace ?p1 ?p2)
                    (salary ?p1 ?s1)
                    (salary ?p2 ?s2)
                    (lisp-value < ?s2 ?s1))))

実行結果
;; ;;; Query input:
;; (replace ?x (fect cy d))
;;
;; ;;; Query results:
;; (replace (bitdiddle ben) (fect cy d))
;; (replace (hacker alyssa p) (fect cy d))
;;
;; ;;; Query input:
;; (ex4.57.b ?p ?q ?r ?s)
;;
;; ;;; Query results:
;; (ex4.57.b (bitdiddle ben) 60000 (tweakit lem e) 25000)
;; (ex4.57.b (bitdiddle ben) 60000 (fect cy d) 35000)
;; (ex4.57.b (bitdiddle ben) 60000 (hacker alyssa p) 40000)
;; (ex4.57.b (hacker alyssa p) 40000 (fect cy d) 35000)
;;4.58
#|
(assert! (rule (big-shot ?p ?j)
               (and (job ?p (?j . ?x))
                    (supervisor ?p ?q)
                    (job ?q (?k . ?y))
                    (not (same ?j ?k)))))



;; 実行例
;; ;;; Query input:
;; (big-shot ?p ?j)
;;
;; ;;; Query results:
;; (big-shot (scrooge eben) accounting)
;; (big-shot (bitdiddle ben) computer)
;;
;;4.59
;;(assert! (meeting accounting (Monday 9am)))
;;(assert! (meeting administration (Monday 10am)))
#||
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

(assert! (rule (meeting-time ?person ?day-and-time)
               (and (job ?person (?x . ?y))
                    (or (meeting ?x ?day-and-time)
                        (meeting whole-company ?day-and-time)))))

;; 実行結果
;; ;;; Query input:
;;
;; (meeting ?x (Friday . ?y))
;;
;; ;;; Query results:
;; (meeting administration (friday 1pm))
;;
;; ;;; Query input:
;; (meeting-time (hacker alyssa p) ?day-and-time)
;;
;; ;;; Query results:
;; (meeting-time (hacker alyssa p) (wednesday 3pm))
;; (meeting-time (hacker alyssa p) (wednesday 4pm))
;;
;; ;;; Query input:
;;
;; (meeting-time (hacker alyssa p) (wednesday . ?t))
;;
;; ;;; Query results:
;; (meeting-time (hacker alyssa p) (wednesday 3pm))
;; (meeting-time (hacker alyssa p) (wednesday 4pm))




#|
4.60
(define (s= a b)
  (string=? (symbol->string a) (symbol->string b)))
(define (s< a b)
  (stringstring a) (symbol->string b)))

(define (l< a b)
 (cond ((null? a) (not (null? b)))
       ((null? b) #f)
       ((s= (car a) (car b)) (l< (cdr a) (cdr b)))
       (else (s< (car a) (car b)))))

(assert! (rule (same ?x ?x)))

(assert! (rule (lives-near ?p1 ?p2)
(and (address ?p1 (?t1 . ?x1))
     (address ?p2 (?t1 . ?x2))
     (not (same ?p1 ?p2))
     (lisp-value l< ?p1 ?p2))))
 4.61
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
               (?x next-to ?y in ?z)))
実行例
 Query input:
(?x next-to ?y in (1 (2 3) 4))
 
 Query results:
 ((2 3) next-to 4 in (1 (2 3) 4))
 (1 next-to (2 3) in (1 (2 3) 4))
 
 Query input:
(?x next-to 1 in (2 1 3 1))

 Query results:
 (3 next-to 1 in (2 1 3 1))
 (2 next-to 1 in (2 1 3 1))

問題 4.62
(assert! (rule (last-pair (?x) (?x))))
(assert! (rule (last-pair (?x . ?y) ?z)
               (last-pair ?y ?z)))


問題 4.64
                                        ;もともとの規則は
(assert! (rule (outranked-by ?staff-person ?boss)
               (or (supervisor ?staff-person ?boss)
                   (and (supervisor ?staff-person ?middle-manager)
                        (outranked-by ?middle-manager ?boss)))))

;;; Query input:

(outranked-by (hacker alyssa p) ?x)

;;; Query results:
(outranked-by (hacker alyssa p) (bitdiddle ben))
(outranked-by (hacker alyssa p) (warbucks oliver))

;;; Query input:
(outranked-by (reasoner louis) ?x)

;;; Query results:
(outranked-by (reasoner louis) (hacker alyssa p))
(outranked-by (reasoner louis) (bitdiddle ben))
(outranked-by (reasoner louis) (warbucks oliver))

                                        ;新しい定義
(assert! (rule (outranked-by2 ?staff-person ?boss)
               (or (supervisor ?staff-person ?boss)
                   (and (outranked-by2 ?middle-manager ?boss)
                        (supervisor ?staff-person ?middle-manager)))))

                                        ;outranked-by2がout-ranked-by2を再帰呼び出しするので無限に走る.

;;4.65
(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))

(wheel ?who)


((?person) warbucks oliver) (?middle-manager) aull dewitt) (?who) ?person))
((?person) scrooge eben) (?middle-manager) cratchet robert) (?who) ?person))
((?person) warbucks oliver) (?middle-manager) scrooge eben) (?who) ?person))
((?person) warbucks oliver) (?middle-manager) bitdiddle ben) (?who) ?person))
((?person) hacker alyssa p) (?middle-manager) reasoner louis) (?who) ?person))
((?person) bitdiddle ben) (?middle-manager) tweakit lem e) (?who) ?person))
((?person) bitdiddle ben) (?middle-manager) fect cy d) (?who) ?person))
((?person) bitdiddle ben) (?middle-manager) hacker alyssa p) (?who) ?person))

(supervisor ?middle-manager ?person)
でデーターベースを見ると上の8つのフレームが出来る.
その環境で
パターンを
(supervisor ?x ?middle-manager)
としてデーターベースを見直す.

1行目 フェイル
2行目 フェイル
3行目 (supervisor (Cratchet Robert) (Scrooge Eben)) でマッチ
→ ((?person) warbucks oliver))を出力
4行目 (supervisor (Hacker Alyssa P) (Bitdiddle Ben)) でマッチ
→ ((?person) warbucks oliver))を出力
(supervisor (Fect Cy D) (Bitdiddle Ben)) でマッチ
→ ((?person) warbucks oliver))を出力
(supervisor (Tweakit Lem E) (Bitdiddle Ben))) でマッチ
→ ((?person) warbucks oliver))を出力
5行目 フェイル
6行目 フェイル
7行目 フェイル
8行目 (supervisor (Reasoner Louis) (Hacker Alyssa P)) でマッチ
→ ((?person) bitdiddle ben))を出力

結果として warbucks oliver が 4回 bitdiddle ben が1回 出力される.


;;4.66
問題4.65の結果からわかるように, 例えばwheelの給料の和を計算しようとすると,
Warbucks Oliverの給料を4回加算してしまう.

フレイムに同一の内容は1回しかあらわれないようにしたい.
;;4.67
(assert! (rule (outranked-by ?sp ?b)
               (or (supervisor ?sp ?b)
                   (and (outranked-by ?mm ?b)
                        (supervisor ?sp ?mm) ))))

と定義して

(outranked-by (bitdiddle ben) ?w)

(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?w))
               (append-to-form ?v ?y ?w)))
(append-to-form (a b) (c d) ?x)



を実行するとループに入る. これを防ぐには
(define qhistory '())
とし, query-loopに入る度に(set! qhistory '())とリセットし,
queryのなかで queryがqhistoryのメンバーであるかを調べる.
ただし変数には番号がついているので, それを無視したい. そのため

(define (hmember a l)
  (if (null? l) '()
      (or (hequal? a (car l)) (hmember a (cdr l)))))

(define (hequal? a b)
  (or (eq? a b)
      (and (number? a) (number? b))
      (and (pair? a) (pair? b) (hequal? (car a) (car b))
           (hequal? (cdr a) (cdr b)))))

を使う.

(define (qeval query frame-stream)
  (cond ((not (hmember query qhistory))   ;;queryがqhistoryになければ
         (set! qhistory (cons query qhistory))   ;;queryをqhistoryに追加し
         (let ((qproc (get (type query) 'qeval))) ;;通常に実行する.
           (if qproc
               (qproc (contents query) frame-stream)
               (simple-query query frame-stream))))
        (else (error "query loop" query))))     ;;qhistoryにあればerrorでとまる.


実行すると
;;; Query input:
(outranked-by (bitdiddle ben) ?w)

;;; Query results:
(outranked-by (bitdiddle ben) (warbucks oliver))

そのうち

;;qeval
(or (supervisor (? 2 sp) (? 2 b)) (and (outranked-by (? 2 mm) (? 2 b)) (supervisor (? 2 sp) (? 2 mm))))

を評価しようとし,

;;qhist (
(outranked-by (? 1 mm) (? 1 b)
              )
(and (outranked-by (? 1 mm) (? 1 b)) (supervisor (? 1 sp) (? 1 mm)))
(supervisor (? 1 sp) (? 1 b))
(or (supervisor (? 1 sp) (? 1 b)) (and (outranked-by (? 1 mm) (? 1 b)) (supervisor (? 1 sp) (? 1 mm))))
(outranked-by (bitdiddle ben) (? w))
)
のようにqhistoryにあるので停止する.

                                        ;query loop (or (supervisor ... ...) (and ... ...))
                                        ;To continue, call RESTART with an option number:
                                        ; (RESTART 1) => Return to read-eval-print level 1.
問題 4.68
(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x . ?y) ?z)
               (and (reverse ?y ?w)
                    (append-to-form ?w (?x) ?z))))


(append-to-form (a b) (c d) ?z)

(reverse (1 2 3) ?x)
問題 4.69
;; データーベース
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

;; grandsonの規則を書き直す((grandson) ?x ?y)の形にする.

(assert! (rule ((grandson) ?g ?s) (and (son ?f ?s) (son ?g ?f))))

;; リストの最後がgrandsonで終ることを見る規則

(assert! (rule (gsl (grandson))))
(assert! (rule (gsl (?x . ?y))
               (gsl ?y)))

;; (great . ?rel)の規則

(assert! (rule ((great . ?rel) ?x ?y)
               (and (son ?z ?y) (?rel ?x ?z) (gsl ?rel))))

;; 実行結果
;; ;;; Query input:
;; ((great grandson) ?g ?ggs)
;;
;; ;;; Query results:
;; ((great grandson) irad lamech)
;; ((great grandson) enoch methushael)
;; ((great grandson) cain mehujael)
;; ((great grandson) adam irad)
;;
;; ;;; Query input:
;; (?relationship Adam Irad)
;;
;; ;;; Query results:
;; ((great grandson) adam irad)
;;
;; ;;; Query input:
;; (?relationship adam lamech)
;;
;; ;;; Query results:
;; ((great great great great grandson) adam lamech)
;;


