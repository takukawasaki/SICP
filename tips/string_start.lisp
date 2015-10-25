(defun starts-with (start sequence)
  (let (( p (search start sequence)))
    (and p (= 0 p))))


(defun ends-with (end sequence)
  (let ((p (mismatch end sequence :from-end T)))
    (or (not p) (= 0 p))))



(defun containsp (str1 contains)
    "Determine whether `str1` contains `str2`.
   Instead of just returning T, return a list of starting locations
   for every occurence of `str2` in `str1`"
    (unless (string-equal contains "")
      (loop
         for p = (search contains str1)
         then (search contains str1 :start2 (incf p))
         while p
         collect p)))

