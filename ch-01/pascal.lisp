(defun pascal (n i)
  (if (or (= i 0) (= i n))
      1
      (+ (pascal (- n 1) (- i 1))
	 (pascal (- n 1) i))))


(defun mult (x coll)
  (let ((result nil))
    (if (null coll)
        result
        (setf result
              (cons (* x (first coll))
                    (mult x (rest coll)))))))
          



