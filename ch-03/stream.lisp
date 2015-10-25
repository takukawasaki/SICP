(require 'set "~/Dropbox/SICP/BYLISP/ch-03/changeable-list.lisp")
(require 'para "~/Dropbox/SICP/BYLISP/ch-03/parallels.lisp")
(require 'digi "~/Dropbox/SICP/BYLISP/ch-03/digital-simulator.lisp")


(defun cons-stream (a proc)
  (cons a  proc))

(defun delay (x)
  #'(lambda () x))

(defvar the-empty-stream nil)

(defun stream-null? (stream)
  (null stream))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(defun stream-map (proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(defun stream-for-each (proc s)
  (if (stream-null? s)
      'done
      (progn
        (proc (stream-car s)
              (stream-for-each proc (stream-cdr s))))))

(defun display-stream (s)
  (stream-for-each display-line s))

(defun display-line (x)
  (fresh-line)
  (princ x))

(defun force (delayed-object)
  (funcall delayed-object))


(defun stream-car (stream)
  (car stream))

(defun stream-cdr (stream)
  (force (cdr stream)))



(defun stream-filter (pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((funcall pred (stream-car stream))
         (cons-stream (stream-car stream)
                      #'(lambda ()
                          (stream-filter pred
                                         (stream-cdr stream)))))
        (t
         (stream-filter pred (stream-cdr stream)))))


(defun stream-enumerate-interval (low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low #'(lambda ()(stream-enumerate-interval (+ low 1) high)))))



(defun memo-proc (proc)
  (let ((already-run? nil)
        (result nil))
    #'(lambda ()
        (if (not already-run?)
            (progn
              (setf result (funcall proc))
              (setf already-run? t)
              result)
            result))))

;;3-50

(defun stream-map (proc &rest args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (mapcar #'stream-car args))
       #'(lambda ()
           (apply #'stream-map
                  (cons proc (mapcar #'stream-cdr args)))))))


(defun show (x)
  (princ x)
  x)


(defun integer-start-from-n (n)
  (cons-stream n #'(lambda () (integer-start-from-n (+ n 1)))))

(defvar integers (integer-start-from-n 1))

(defun visible? (x y)
  (= (mod x y) 0))


(defvar no-sevens (stream-filter #'(lambda (x) (not (visible? x 7)))
                                 integers))

(defun sieve (stream)
  (cons-stream
   (stream-car stream)
   #'(lambda ()
       (sieve (stream-filter
               #'(lambda (x)
                   (not (visible? x (stream-car stream))))
               (stream-cdr stream))))))

(defvar primes (sieve (integer-start-from-n 2)))

(defun add-stream (s1 s2)
  (stream-map #'+ s1 s2))

(defun repeat (n)
  (cons-stream n #'(lambda () (repeat n))))

(defvar ones (cons-stream 1 #'(lambda () ones)))


(defvar integ (cons-stream 1 #'(lambda ()(add-stream ones  integ))))




(defun scale-stream (stream factor)
  (stream-map (lambda () (* x factor)) stream))


(defvar double (cons-stream 1 (scale-stream double 2)))


(defvar prime2
  (cons-stream 2
               (stream-filter prime? (integer-start-from-n 3))))

(defun prime? (n)
  (labels ((iter (ps)
             (cond ((> (square (stream-car ps)) n) t)
                   ((divisible? n (stream-car ps)) nil)
                   (t
                    (iter (stream-cdr ps))))))
    (iter primes)))

;;3-54



(defvar factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))


;;3-55

(defun partial-sums (s)
  (lambda () s1 (cons-stream (stream-car s) (add-streams (stream-cdr s) s1)))
  (funcall s1))


;;3-57
(defun merge (s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (t
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge (steam-cdr s2))))
                 (t
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))


(defvar S (cons-stream 1 (merge (stream-map *3 S)
                                (merge (stream-map *5 S)
                                       (stream-map *7 S)))))

;;3-60
(defun mul-series (s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (stream-map
                             #'(lambda (x) (* (stream-car s1) x))
                             (stream-cdr s2))
                            (mul-series (stream-cdr s1) s2))))

;;3-61

(defun invert-unit-series (sr)
    (let ((x (cons-stream 1 (stream-map - (mul-series sr x)))))
      x))



;;3-62
(defun div-series (s1 s2)
  (let ((constant-term (stream-car s2)))
    (if (= constant-term 0) (error "divisor's constant term is zero" (stream-car s2))
        (stream-map (lambda (x) (/ x constant-term))
                    (mul-series s1 (invert-unit-series
                                    (stream-map (lambda (x) (/ x constant-term))
                                                s2)))))))

;;3-64
(defun stream-limit (stream tolerance)
  (let ((first (stream-car stream)) (rest (stream-cdr stream)))
    (if (< (abs (- first (stream-car rest))) tolerance)
        first
        (stream-limit rest tolerance))))

(defun sqrt-stream (x)
  (labels ((guesses ()
             (cons-stream 1.0
                          (stream-map (lambda (guess)
                                        (sqrt-impreve guess x))
                                      guesses))))
  guesses))

(defun sqrt-impreve (guess x)
    (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun sqrt (x tolerance)
  (stream-limit (sqrt-stream x) tolerance))


;;3-66

(defun interleave (s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))


(defun pairs (s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


;;3-70

(defun merge-weighted (weight s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (t
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (< (apply weight s1car) (apply weight s2car))
               (cons-stream s1car (merge-weighted weight (stream-cdr s1) s2))
               (cons-stream s2car (merge-weighted weight s1 (stream-cdr s2))))))))

(defun weighted-pair (weight s1 s2)
  (cons-stream (list (stream-car s1) (stream-car s2))
               (merge-weighted weight (stream-map (lambda (x) (list (stream-car s1) x))
                                                  (stream-cdr s2))
                               (weighted-pair weight (stream-cdr s1)
                                              (stream-cdr s2)))))


;;3-71


(defun find-twin (stream)
  (let ((first (stream-car stream)) (rest (stream-cdr stream))
        (second (stream-car (stream-cdr stream))))
    (let ((i0 (car first)) (j0 (cadr first))
          (i1 (car second)) (j1 (cadr second)))
      (cond ((= (+ (* i0 i0 i0) (* j0 j0 j0)) (+ (* i1 i1 i1) (* j1 j1 j1)))
             (display first) (display second) (newline)))
      (find-twin rest))))

(find-twin
  (weighted-pair (lambda (i j) (+ (* i i i) (* j j j))) integers integers))


;;3=72


(defun find-triplet (stream)
  (let ((first (stream-car stream)) (rest (stream-cdr stream))
        (second (stream-car (stream-cdr stream)))
        (third (stream-car (stream-cdr (stream-cdr stream)))))
    (let ((i0 (car first)) (j0 (cadr first))
          (i1 (car second)) (j1 (cadr second))
          (i2 (car third)) (j2 (cadr third)))
      (cond ((and
              (= (+ (* i0 i0) (* j0 j0)) (+ (* i1 i1) (* j1 j1)))
              (= (+ (* i0 i0) (* j0 j0)) (+ (* i2 i2) (* j2 j2))))
             (display first) (display second) (display third) (newline)))
      (find-triplet rest))))

(find-triplet
 (weighted-pair (lambda (i j) (+ (* i i) (* j j))) integers integers))



(defun make-simple (balance)
  (lambda (amount)
    (setf balance (- balance amount))
    balance))



(defun integral (integrand initial-value dt)
    (defvar int
      (cons-stream initial-value
                   (add-streams (scale-stream integrand dt)
                                int)))
  int)

(defun RC (R C dt)
  (lambda (i-stream vo)
    (cons-stream v0
                 (add-streams (scale-stream i-stream R)
                              (integral (scale-stream i-stream (/ 1.0 C)) v0)))))

(defvar RC1 (RC 5 1 0.5))


;;3-74

(defun list->stream (list)
  (if (null list) the-empty-stream
      (cons-stream (car list) (list->stream (cdr list)))))

(defvar sense-data
    (list->stream
     '(1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4)))

(defun sign-change-detector (next last)
  (cond ((and (>= next 0) (< last 0)) 1)
        ((and (< next 0) (>= last 0)) -1)
        (t 0)))

(defun make-zero-crossings (input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream
       (sign-change-detector (stream-car input-stream) last-value)
       (make-zero-crossings (stream-cdr input-stream)
                            (stream-car input-stream)))))

(defvar zero-crossings (make-zero-crossings sense-data 0))


;;3-75


(defun list->stream (list)
    (if (null? list) the-empty-stream
        (cons-stream (car list) (list->stream (cdr list)))))

(defvar sense-data
    (list->stream
     '(1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4)))


(defun sign-change-detector (next last)
  (cond ((and (>= next 0) (< last 0)) 1)
        ((and (< next 0) (>= last 0)) -1)
        (t 0)))

(defun make-zero-crossings (input-stream last-value last-avpt)
  (if (stream-null? input-stream)
      the-empty-stream
      (let ((avpt (/ (+ (stream-car input-stream) last-value) 2))
            (next-value (stream-car input-stream)))
        (cons-stream (sign-change-detector avpt last-avpt)
                     (make-zero-crossings (stream-cdr input-stream)
                                          next-value
                                          avpt)))))

(defvar zero-crossings (make-zero-crossings sense-data 0 0))

;;3-76
(defun list->stream (list)
    (if (null? list) the-empty-stream
        (cons-stream (car list) (list->stream (cdr list)))))

(defvar sense-data
    (list->stream
     '(1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4)))

(defun sign-change-detector (next last)
    (cond ((and (>= next 0) (< last 0)) 1)
          ((and (< next 0) (>= last 0)) -1)
          (t 0)))



(defun smooth (input-stream last-value)
    (if (stream-null? input-stream) the-empty-stream
        (let ((av (/ (+ (stream-car input-stream) last-value) 2)))
          (cons-stream av
                       (smooth (stream-cdr input-stream) (stream-car input-stream))))))


(defun make-zero-crossings (input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream (sign-change-detector (stream-car input-stream) last-value)
                   (make-zero-crossings (stream-cdr input-stream)
                                        (stream-car input-stream)))))

(defvar zero-crossings (make-zero-crossings
                        (smooth sense-data 0) 0))


;;3-77

(defun integral (delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

(defun solve (f y0 dt)
  (defvar y (integral (delay dy) y0 dt))
  (defvar dy (stream-map f y)))


;;3-78

(defun solve-2nd (a b y0 dy0 dt)
    (defvar y (integral (delay dy) y0 dt))
    (defvar dy (integral (delay ddy) dy0 dt))
    (defvar ddy (add-streams (scale-stream dy a) (scale-stream y b)))
    y)



;;3-79

(defun solve-2nd (f y0 dy0 dt)
  (labels ((y () (integral (delay (dy)) y0 dt))
           (dy () (integral (delay (ddy)) dy0 dt))
           (ddy () (stream-map f (dy) (y))))
    (y)))

;;3-80

(defun RLC (R L C dt)
  (labels ((rlc-model (vc0 il0)
             (labels ((il ()
                        (integral (delay (dil)) il0 dt))
                      (vc ()
                        (integral (delay (dvc)) vc0 dt))
                      (dil ()
                        (add-streams
                         (scale-stream (vc) (/ 1 L))
                         (scale-stream (il) (- (/ R L)))))
                      (dvc ()
                        (scale-stream (il) (/ -1 C))))
               (cons (vc) (il)))))
        #'rlc-model))

