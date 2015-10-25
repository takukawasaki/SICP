(defun square (n)
  (* n n))

(defun true () t)
(defun false () nil)

(defun sum-of-square (x y)
  (+ (square x) (square y)))

(defun f (a)
  (sum-of-square (+ a 1) (* a 2)))

(defun sum-of-two-number (a b c)
  (cond ((and (> a b) (> b c)) (+ (square a) (square b)))
	((and (> b a) (> c a)) (+ (square b) (square c)))
	((t (+ (square a) (square c))))))

(defun sqrt-iter (guess x)
  (if (good-enough guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(defun improve (guess x)
  (average guess (/ x guess)))

(defun average (x y)
  (/ (+ x y) 2))

(defun good-enough (guess x)
  (< (abs (- (square guess) x)) 0.0001))

(defun sqrt-00 (x)
  (sqrt-iter 1.0 x))

(defun cbert-iter (guess x)
  (if (good-enough2 guess x)
      guess
      (cbert-iter (improve2 guess x) x)))

(defun cube (x)
  (* x x x))

(defun good-enough2 (guess x)
  (< (abs (- (cube guess) x)) 0.001))

(defun improve2 (guess x)
  (/ (+ (/ x (square guess))
	(* 2 guess))
     3))

(defun cbert-00 (x)
  (cbert-iter 1.0 x))
