(defstruct delay (value nil) (function nil))


(defmacro delay (&rest body)
  `(make-delay :function #'(lambda () . ,body)))

(defstruct me (first  nil)(last nil) )


(defun force (x)
  (if (not (delay-p x))
      x
      (progn
        (when (delay-function x)
          (setf (delay-value x)
                (funcall (delay-function x)))
          (setf (delay-function x) nil))
        (delay-value x))))


(defmacro cons-pipe (head tail)
  "create a pipe by evaluate head and delaying tail"
  `(cons ,head (delay ,tail)))

(defconstant empty-pipe nil)


(defun head (strm) (first strm))
(defun tails (strm) (force (rest strm)))


(defun stream-elt (strm i)
  "0base i-th element of stream "
  (if (= i 0)
      (head strm)
      (stream-elt (tail strm) (- i 1))))


(defun integers (&optional (start 0) end)
  "A stream of integers from start to end"
  (if (or (null end) (<= start end))
      (cons-pipe start (integers (+ start 1) end))
      nil))


(defmacro cons-stream (head tail)
  `(cons ,head #'(lambda () ,tail)))

(defun tail (strm)
  "return tail of pipe or list ,and destructively update
   the tail if it is a function"
  (if (functionp (rest strm))
      (setf (rest strm) (funcall (rest strm)))
      (rest strm)))

(defun integers2 (&optional (start 0) end)
  "A stream of integers from start to end"
  (if (or (null end) (<= start end))
      (cons-stream start (integers2 (+ start 1) end))
      nil))




(defun enum-stream (strm &key count key (result strm))
  "Go through all element of stream.
   Possibly applying the key function "
  (if (or (eq strm empty-pipe) (eql count 0))
      result
      (progn
        (unless (null key) (funcall key (head strm)))
        (enum-stream (tail strm) :count (if count (- count 1))
                   :key key :result result))))

(defun filter (pred strm)
  (if (funcall pred (head strm))
      (cons-stream (head strm)
                   (filter pred (tail strm)))
      (filter pred (tail strm))))

(defun sieve (strm)
  (cons-stream (head strm)
               (filter #'(lambda (x) (/= (mod x (head strm)) 0))
                       (sieve (tail strm)))))


(defvar *primes* (sieve (integers2 2)))
(enum-stream *primes* :count 4 :key #'(lambda (x) (print (* x x))))

(defun map-stream (fn strm)
  (if (eq strm empty-pipe)
      empty-pipe
      (cons-stream (funcall fn (head strm))
                   (map-stream fn (tail strm)))))

(defun append-stream (x y)
  (if (eq x empty-pipe)
      y
      (cons-stream (head x)
                   (append-stream (tail x) y))))

(defun mappend-stream (fn strm)
  (if (eq strm empty-pipe)
      empty-pipe
      (let ((x (funcall fn (head strm))))
        (cons-stream (head x)
                     (append-stream (tail x)
                                    (mappend-stream
                                     fn (tail strm)))))))

                      
