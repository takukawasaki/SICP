(define nil '())



(define (cons-stream a proc)
  (cons a proc))

(define (delay x)
  (lambda () x))

(define the-empty-stream nil)


(define (stream-null? stream)
  (null? stream))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-cdr s)
              (stream-for-each proc (stream-cdr s))))))


(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (force delayed-object)
  (delayed-object))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (lambda ()
                        (stream-filter pred
                                       (stream-cdr stream)))))
        (else
         (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (lambda () (stream-enumerate-interval (+ low 1) high)))))

(define (memo-proc proc)
  (let ((already-run? nil)
        (result nil))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result proc)
            (set! already-run? #t)
            result)
          result))))


(define (stream-map proc . args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car args))
       (apply stream-map
              (cons proc (map stream-cdr args))))))


(define (show x)
  (display x)
  x)

(define (integer-start-from-n n)
  (cons-stream n (lambda () (integer-start-from-n (+ n 1)))))

(define integers (integer-start-from-n 1))

(define (visible? x y)
  (= (remainder x y) 0))


(define (repeat n)
  (cons-stream n (lambda ()(repeat n))))


(define ones (repeat 1))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (delay (sieve (stream-filter
                  (lambda (x)
                    (not (visible? x (stream-car stream))))
                  (stream-cdr stream))))))



(define (add-stream s1 s2)
  (stream-map + s1 s2))

(define integer (cons-stream 1 (add-stream ones integers)))

(define prime (sieve (integer-start-from-n 2)))

