(defun cesaro-test ()
  (= (gcd (rand) (rand)) 1))

(defun monte-carlo (trials experiment)
  (labels ((iter (trials-remaining trials-passed)
             (cond ((= trials-remaining 0)
                    (/ trials-passed trials))
                   ((experiment)
                    (iter (- trials-remaining 1) (+ trials-passed 1)))
                   (t
                    (iter (- trials-remaining 1) trials-passed)))))
    (iter trials 0)))

(defun estimate-pi (trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))


(defun random-gcd-test (trials initial-x)
  (labels ((iter (trials-remaining trials-passed)
             (let ((x1 (rand-updata x)))
               (let ((x2 (rand-updata x1)))
                 ))))))
;;3-5
(defun random-range (low high)
  (let ((range (- high low)))
    (+ low (random range))))


;;3-6


(defvar rand
  (let ((x (random 10)))
    (lambda (symbol)
      (case symbol
        ('genarate (set! x (rand-update x)) x)
        ('reset (lambda (new-value) (set! x new-value) x))
        (t (error "Unknown symbol --- RAND" symbol)))) ))


(defun make-simple-withdraw (balance)
  #'(lambda (amount)
      (setf balance (- balance amount))
      balance))



(defun factorial (n)
  (labels ((iter (product counter)
             (if (> counter n)
                 product
                 (iter (* product counter)
                       (+ counter 1)))))
    (iter 1 1)))



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


;;3-3

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

(defun set-amt (object type &rest args)
  (apply (set-t  object type) args))

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

;;3-7
(defun make-account-2 (balance password)
  (let ((pass password)
        (pass2 password))
    (lambda (type)
      (case type
        (withdraw #'(lambda (amt pw)
                      (if (and (>= balance amt) (or (eq pw pass)(eq pw pass2)))
                          (progn (setf balance (- balance amt))
                                 balance)
                          "incorrect password")))
        (deposit #'(lambda (amt pw)
                     (if (or (eq pw pass) (eq pw pass2))
                         (progn (setf balance (+ balance amt))
                                balance)
                         "incorrect password")))
        (balance #'(lambda (pw)
                     (if (or (eq pw pass) (eq pw pass2))
                         balance
                         "incorrect password")))
        (joint #'(lambda (pw nwpass)
                   (if (or (eq pw pass) (eq pw pass2))
                       (progn (setf pass2 nwpass)
                              (setf  acct (make-account-2 balance nwpass)))
                       "incorrect pass")))))))
