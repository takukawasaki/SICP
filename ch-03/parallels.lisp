(require 'set "~/Dropbox/SICP/BYLISP/ch-03/changeable-list.lisp")




(defun make-account (balance)
  (labels ((withdraw (amt)
             (if (>= balance amt)
                 (progn
                   (setf balance (- balance amt))
                   balance)
                 "no enough balance"))
           (deposit (amt)
             (setf balance (+ balance amt))
             balance)
           (dispatch (m)
             (let ((protected (make-serializer)))
               (cond ((eq m 'withdraw) (protected withdraw))
                     ((eq m 'deposit) (protected deposit))
                     ((eq m 'balance) balance)
                     (t (error "Unknown request -- Make Account" m))))))
    #'dispatch))

(defun exchange (account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))


(defun make-account (balance)
  (labels ((withdraw (amt)
             (if (>= balance amt)
                 (progn
                   (setf balance (- balance amt))
                   balance)
                 "no enough balance"))
           (deposit (amt)
             (setf balance (+ balance amt))
             balance)
           (dispatch (m)
             (let ((balance-serializer (make-serializer)))
               (cond ((eq m 'withdraw) withdraw)
                     ((eq m 'deposit)  deposit)
                     ((eq m 'balance) balance)
                     ((eq m 'serializer) balance-serializer)
                     (t (error "Unknown request -- Make Account" m))))))
    #'dispatch))

(defun serializer-exchange (account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))

(defun make-serializer ()
  (let ((mutex (make-mutex)))
    #'(lambda (p)
        (labels ((serialized-p (&rest args)
                   (funcall mutex 'acquire)
                   (let ((val (apply p args)))
                     (funcall mutex 'release)
                     val)))
          #'seialized-p))))

(defun make-mutex ()
  (let ((cell (list false)))
    (labels ((the-mutex (m)
               (cond ((eq m 'acquire)
                      (if (test-and-set! cell)
                          (the-mutex 'acquire)))
                     ((eq m 'release) (clear! cell)))))
      #'the-mutex)))

(defun clear! (cell)
  (set-car! cell false))

(defun test-and-set! (cell)
  (if (car cell)
      t
      (progn (set-car! cell t)
             nil)))




