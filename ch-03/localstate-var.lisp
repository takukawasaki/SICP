;;3-1-1
(defvar new-withdraw 
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (progn (setf balance (- balance amount))
                 balance)
          "Insufficient funds"))))



(defun make-withdraw (balance)
  (lambda (amount)
    (if (>= balance amount)
        (progn (setf balance (- balance amount))
               balance)
        "Insuffcient funds")))



(defvar w1 (make-withdraw 100))
(defvar w2 (make-withdraw 100))



(defun make-account (balance password)
  (let ((pass password))
    (lambda (type)
      (case type
        (withdraw #'(lambda (amt pw)
                      (if (and (>= balance amt) (eq pw password))
                          (progn (setf balance (- balance amt))
                                 balance)
                          "incorrect password")))
        (deposit #'(lambda (amt pw)
                     (if (eq pw password)
                         (progn (setf balance (+ balance amt))
                                balance)
                         "incorrect password")))
        (balance #'(lambda (pw)
                     (if (eq pw password)
                         balance
                         "incorrect password")))))))

(defun set-amt (object type &rest amt)
  (apply (funcall  object type) amt))

(defun make-accumulator (value)
  (lambda (amt)
    (setf value  (+ value amt))))

(defun set-t (object amt)
  (funcall object amt))

(defun make-monitor (f)
  (let ((count 0))
    (lambda (x)
      (cond ((numberp x)
             (setf count (+ count 1))
             (funcall f x))
            ((string= x "how many times")
             count)))))




;;3-4

(defun call-the-cops () (format t"call-the-cops"))

(defun make-account-1 (balance password )
  (let ((invalid 0))
    (lambda (type)
      (if (>= invalid 7)
          (progn (call-the-cops)
                 (setf invalid-count 0))
          (case type
            (withdraw #'(lambda (amt pw)
                          (if (and (>= balance amt) (eq pw password))
                              (progn (setf balance (- balance amt))
                                     balance)
                              (progn (incf invalid)
                                     "incorrect password"))))
            (deposit #'(lambda (amt pw)
                         (if (eq pw password)
                             (progn (setf balance (+ balance amt))
                                    balance)
                             (progn (incf invalid)
                                    "incorrect password"))))
            (balance #'(lambda(pw)
                         (if (eq pw password)
                             balancen
                             "incorrect password"))))))))



