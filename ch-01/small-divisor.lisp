(defun smallest-divisor (n)
  (find-divisor n 2))

(defun find-divisor (n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides n test-divisor)
         test-divisor
         0)
        (t (find-divisor n (+ test-divisor 1)))))

(defun divides (a b)
  (= (mod a b) 0))

(defun prime (n)
  (= n (smallest-divisor n)))

(defun square (x)
	(* x  x))



