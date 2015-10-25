(defun gdc (a b)
  (if (= b 0)
      a
      (gdc b (mod a b))))
