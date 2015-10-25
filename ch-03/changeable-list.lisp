
(defun set-car! (L elem)
  (setf (car L) elem))


(defun set-cdr! (L elem)
  (setf (cdr L) elem))



;;3-12

(defun appendee (x y)
  (if (null x)
      y
      (cons (car x) (appendee (cdr x) y))))

(defun append! (x y)
  (set-cdr! (last-pair x) y)
  x)

(defun last-pair (x)
  (if (null (cdr x))
      x
      (last-pair (cdr x))))



(defun make-cycle (x)
  (set-cdr! (last-pair x) x)
  x)




;;3-14
;;reverse 
(defun mystery (x)
  (labels ((loops (x y)
              (if (null x)
                  y
                  (let ((temp (cdr x)))
                    (set-cdr! x y)
                    (loops temp x)))))
    (loops x '())))

(defun set-to-wow! (x)
  (set-car! (car x) 'wow)
  x)

;;3-16

(defun count-pairs (x)
  (let ((tracelist nil))
    (labels ((counting (x)
               (cond ((not (consp x)) 0)
                     ((member x tracelist) 0)
                     (t
                      (setf tracelist (cons x tracelist))
                      (+ (counting (car x))
                         (counting (cdr x))
                         1)))))
      (counting x))))


;;3-18
(defun find-cycle (list)
  (let ((tracelist '()))
    (labels ((trace (list)
               (if  (member list tracelist)
                    t
                    (progn
                      (setf tracelist (cons list tracelist))
                      (if (consp list)
                          (progn (trace (car list))
                                 (trace (cdr list)))
                          '()))))
             (trace list)))))


(defun conso (x y)
  #'(lambda (m) (funcall m x y)))

(defun cars (v)
  (funcall v #'(lambda (p q) p)))

(defun cdrs (v)
  (funcall v #'(lambda (p q) q)))

(defun conzo (x y)
  (labels ((set-x! (v) (setf x v))
           (set-y! (v) (setf y v))
           (dispatch (m)
             (cond ((eq m 'car) x)
                   ((eq m 'cdr) y)
                   ((eq m 'set-car!) set-car!)
                   ((eq m 'set-cdr!) set-cdr!)
                   (t
                    (error "undefined" m)))))
    #'dispatch))

(defun carz (z)
  (funcall z 'car))
(defun cdrz (z)
  (funcall z 'cdr))

(defun set-carz! (z new-value)
  ((z set-carz!) new-value)
  z)

(defun f-pointer (queue)
  (car queue))

(defun r-pointer (queue)
  (cdr queue))


(defun set-f-pointer! (queue item)
  (set-car! queue item))

(defun set-r-pointer! (queue item)
  (set-cdr! queue item))

(defun empty-queue? (queue)
  (null (f-pointer queue)))


(defun make-q ()
  (cons nil nil))


(defun front-queue (queue)
  (if (empty-queue? queue)
      (error "front called with an empty queue" queue)
      (car (f-pointer queue))))

(defun insert-queue (queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-f-pointer! queue new-pair)
           (set-r-pointer! queue new-pair)
           queue)
          (t
           (set-cdr! (r-pointer queue) new-pair)
           (set-r-pointer! queue new-pair)
           (print-queue queue)))))


(defun delete-queue (queue)
  (cond ((empty-queue? queue)
         (error "Delete called with an empty queue" queue))
        (t
         (set-f-pointer! queue (cdr (f-pointer queue)))
         (print-queue queue))))

(defun print-queue (queue)
  (fresh-line)
  (princ (f-pointer queue)))

;;3-22

(defun make-queue ()
  (let ((front-ptr '())
        (rear-ptr '()))
    (labels ((front () (car front-ptr))
             (delete! ()
               (if (empty?)
                   (error "DELETE! called with an empty queue" queue)
                   (setf front-ptr (cdr front-ptr))))
             (empty? ()
               (null front-ptr))
             (insert! (item)
               (let ((new-item (list item)))
                 (cond ((empty?) (setf front-ptr new-item)
                        (set! rear-ptr new-item))
                       (t (set-cdr! rear-ptr new-item)
                          (set! rear-ptr (cdr rear-ptr))))))
             (dispatch (m)
               (cond ((eq m 'front) front)
                     ((eq m 'delete!) delete!)
                     ((eq m 'empty?) empty?)
                     ((eq m 'insert!) insert!)
                     (t (error "Undefund operation" m)))))
      #'dispatch)))

;;3-23


(defun queue-contents (q) (cdr q))

(defun make-queues ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)

(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))


(defun lookup (key table)
  (let ((record (azzoc key (cdr table))))
    (if record
        (cdr record)
        nil)))


(defun azzoc (key records)
  (cond ((null records) nil)
        ((equalp key (caar records)) (car records))
        (t (assoc key (cdr records)))))


(defun insert! (key value table)
  (let ((record (azzoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)



(defun make-table1 ()
  (list '*table*))


(defun lookup-2dim (key-1 key-2 table)
  (let ((subtable (azzoc key-1 (cdr table))))
    (if subtable
        (let ((record (azzoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              nil))
        nil)))


(defun insert!-2dim (key-1 key-2 value table)
  (let ((subtable (azzoc key-1 (cdr table))))
    (if subtable
        (let ((record  key-2 (cdr subtable)))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (cons (list key-1
                                    (cons key-2 value))
                              (cdr table))))))
  'ok)


(defun make-table-ret-func ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
               (let ((subtable (azzoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (azzoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable (azzoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (azzoc key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                     (set-cdr! local-table
                               (cons (list key-1
                                           (cons key-2 value))
                                     (cdr local-table)))))
               'ok)
             (dispatch (m)
               (cond ((eq m 'lookup-proc) #'lookup)
                     ((eq m 'insert-proc!) #'insert!)
                     (t (error "Unknown operation -- Table" m)))))
      #'dispatch)))

(defparameter op-table (make-table-ret-func))
(defparameter gets (funcall op-table 'lookup-proc))
(defparameter puts (funcall op-table 'insert-proc!))

;;3-24

(defun make-table-ret-func ()
  (let ((local-table (list '*table*)))
    (labels ((lookup (key-1 key-2)
               (let ((subtable (azzoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (azzoc key-2 (cdr subtable))))
                       (if record
                           (cdr record)
                           nil))
                     nil)))
             (insert! (key-1 key-2 value)
               (let ((subtable (azzoc key-1 (cdr local-table))))
                 (if subtable
                     (let ((record (azzoc key-2 (cdr subtable))))
                       (if record
                           (set-cdr! record value)
                           (set-cdr! subtable
                                     (cons (cons key-2 value)
                                           (cdr subtable)))))
                     (set-cdr! local-table
                               (cons (list key-1
                                           (cons key-2 value))
                                     (cdr local-table)))))
               'ok)
             (dispatch (m)
               (cond ((eq m 'lookup-proc) #'lookup)
                     ((eq m 'insert-proc!) #'insert!)
                     (t (error "Unknown operation -- Table" m)))))
      #'dispatch)))



;;3-25

(defun lookup-list (keylist table)
  (let ((subtable (azzoc (car keylist) (cdr table))))
    (if subtable
        (if (null (cdr keylist))
            (cdr subtable)
            (lookup (cdr keylist) subtable))
        nil)))



(defun insert!-list (keylist value table)
  (let ((subtable (azzoc (car keylist) (cdr table))))
    (if subtable
        (if (null (cdr keylist))
            (set-cdr! subtable value)
            (insert!-list (cdr keylist) value subtable))
        (set-cdr! table
                  (cons (if (null (cdr keylist))
                            (cons (car keylist) value)
                            (let ((newtable (list (car keylist))))
                              (insert! (cdr keylist) value newtable)
                              newtable)) (cdr table))))))


;;3-26

(defun make-table ()
  (list (cons 1000000000 nil) nil nil))

(defun lookup-big (key table)
  (cond ((= key (caar table) (cdar table)))
        ((< key (caar table))
         (if (null (cadr table))
             nil
             (lookup-big key (cadr table))))
        ((> key (caar table))
         (if (null (caddr table))
             nil
             (lookup-big key (caddr table))))))


(defun insert!-big (key value table)
  (cond ((= key (caar table) (set-cdr! (car table) value)))
        ((< key (caar table))
         (if (null (cadr table))
             (set-car! (cdr table) (list (cons key value) nil nil))
             (insert!-big key value (cadr table))))
        ((> key (caar table))
         (if (null (caddr table))
             (set-car! (cddr table) (list (cons key value) nil nil))
             (insert!-big key value (caddr table))))))



;;3-37


(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t
         (+ (fib (- n 1))
            (fib (- n 2))))))


(defun memoize (f)
  (let ((table (make-table)))
    #'(lambda (x)
        (let ((previous-result (lookup x table)))
          (or previous-result
              (let ((result (funcall f x)))
                (insert! x result table)
                result))))))


(defvar memo-fib
  (memoize #'(lambda (n)
               (cond ((= n 0) 0)
                     ((= n 1) 1)
                     (t
                      (+ (fib (- n 1))
                         (fib (- n 2))))))))

