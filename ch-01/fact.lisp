(defun fact (n)
    (if (= n 1)
	1
	(* n (fact (- n 1)))))

(defun fact-fast (n)
  (fact-iter 1 1 n))

(defun fact-iter (product counter max)
  (if (> counter max)
      product
      (fact-iter (* product counter) (+ counter 1) max)))


(defvar L '(1 2 3 4 5))


(defun insert (v index L)
  (cond ((< index 0) (error "index is out of range [index: ~a]" index))
        ((= index 0) (cons v L))
        (t (cons (first L) (insert v (- index 1) (rest L))))))


;;destructive pattern
(defun insert2 (v index seq)
  (if (= index 0)
      (push v seq)
      (push v (cdr (nthcdr (- index 1) seq)))))

        



