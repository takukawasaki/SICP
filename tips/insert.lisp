(setq W '(1 2 3 4 5))


(defun insert (v index L)
  (cond ((< index 0) (error "index is out of range [index: ~a]" index))
        ((= index 0) (cons v L))
        (t (cons (first L) (insert v (- index 1) (rest L))))))


;;destructive pattern
(defun insert2 (v index seq)
  (cond ((< index 0)
         (error "index out of range" ))
        ((= index 0 )
         (push v  seq))
        (t
         (push v (rest (nthcdr (- index 1) seq))))))


;;nested-loop
(defun looper (x y fn)
  (loop for i in x
     collect (loop for j in y
                  collect (funcall fn i j))))

;;range
(defun range (start end &optional (step 1))
  (if (>= start end)
      nil
      (cons start (range (+ start step) end step))))
  
(defvar x (range 0 10))
(defvar y (range 10 20))



