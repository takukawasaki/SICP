(require 'set "~/Dropbox/SICP/BYLISP/ch-03/changeable-list.lisp")


(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(defun inverter (input output)
  (labels ((invert-input ()
             (let ((new-value (logical-not (get-signal input))))
               (after-delay inverter-delay
                            #'(lambda ()
                                (set-signal! output new-value))))))
    (add-action! input #'invert-input))
  'ok)

(defun logical-not (s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (t (error "Invalid signal " s))))

(defun and-gate (a1 a2 output)
  (labels ((and-action-proc ()
             (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
               (after-delay and-gate-delay
                            #'(lambda ()
                                (set-signal! output new-value))))))
    (add-action! a1 and-action-proc)
    (add-action! a2 and-action-proc))
  'ok)

(defun or-gate (a1 a2 output)
  (labels ((or-action-proc ()
             (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
               (after-delay or-gate-delay
                            #'(lambda ()
                                (set-signal! output new-value))))))
    (add-action! a1 #'or-action-proc)
    (add-action! a2 #'or-action-proc))
  'ok)

(defun logical-or (a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((and (= a 1) (= b 1)) 1)
        (t "invalid error" (list a b))))

;;3-29

(defun or-gate2 (a1 a2 output)
  (let ((b1 (make-wire)) (b2 (make-wire)) (c (make-wire)))
    (inverter a1 b1)
    (inverter a2 b2)
    (and-gate b1 b2 c)
    (inverter c output))
  'ok)

;;3-30


(defun ripple-carry-adder (a1 a2 a3 a4 b1 b2 b3 b4 c4 c s1 s2 s3 s4)
  (let ((c1 (make-wire)) (c2 (make-wire)) (c3 (make-wire)))
    (full-adder a4 b4 c4 s4 c3)
    (full-adder a3 b3 c3 s3 c2)
    (full-adder a2 b2 c2 s2 c1)
    (full-adder a1 b1 c1 s1 c)
    'ok))


(defun make-wire ()
  (let ((signal-value 0)
        (action-procedure nil))
    (labels ((set-my-signal! (new-value)
               (if (not (= signal-value new-value))
                   (progn (setf signal-value new-value)
                          (call-each action-procedure))
                   'done))
             (accept-action-procedure! (proc)
               (setf action-procedure (cons proc action-procedure))
               (proc))
             (dispatch (m)
               (cond ((eq? m 'get-signal)
                      signal-value)
                     ((eq? m 'set-signal!)
                      set-my-signal!)
                     ((eq? m 'add-action!)
                      accept-action-procedure!)
                     (t (error "Unknown Operation" m)))))
      #'dispatch)))

(defun call-each (proc)
  (if (null proc)
      'done
      (progn
        ((car proc))
        (call-each (cdr proc)))))

(defun get-signal (wire)
  (wire 'get-signal))

(defun set-signal! (wire new-value)
  ((wire 'set-signal!) new-value))

(defun add-action! (wire action-procedure)
  ((wire 'add-action!) action-procedure))

(defun after-delay (delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action-procedure
                  the-agenda))


(defun propagate ()
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))


(defun probe (name wire)
  (add-action! wire
               #'(lambda()
                   (fresh-line)
                   (princ name)
                   (princ " ")
                   (princ (current-time the-agenda))
                   (princ " New-value =  ")
                   (princ (get-signal wire)))))

(defun make-time-segment (time queue)
  (cons time queue))

(defun segment-time (s)
  (car s))

(defun segment-queue (s)
  (cdr s))

(defun make-agenda () (list 0))

(defun current-time (agenda)
  (car agenda))

(defun set-current-time! (agenda time)
  (set-car! agenda time))

(defun segments (agenda)
  (cdr agenda))

(defun set-segments! (agenda segments)
  (set-cdr! agenda segments))

(defun first-segment (agenda)
  (car (segments agenda)))

(defun rest-segments (agenda)
  (cdr (segments agenda)))

(defun empty-agenda? (agenda)
  (null (segments agenda)))



(defparameter a (make-wire))
(defparameter b (make-wire))
(defparameter c (make-wire))
(defparameter d (make-wire))
(defparameter e (make-wire))
(defparameter s (make-wire))

(defparameter the-agenda (make-agenda))
(defparameter inverter-delay 2)
(defparameter and-gate-delay 3)
(defparameter or-gate-delay 5)
(defparameter input-1 (make-wire))
(defparameter input-2 (make-wire))
(defparameter sum (make-wire))
(defparameter carry (make-wire))

;;3-33

(defun averager (a b c)
  (let ((d (make-connector)) (e (make-connector)))
    (adder a b d)
    (multiplier c e d)
    (constant 2 e)
    'ok))

;;3-37


(defun c+ (x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(defun c- (x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(defun c* (x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(defun c/ (x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(defun cv (x)
  (let ((z (make-connector)))
    (constant x z)
    z))
