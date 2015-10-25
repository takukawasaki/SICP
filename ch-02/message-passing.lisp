
(defun ss (&rest y)
  (labels ((add (z)
	     (apply #'+ z )))
    (add y)))


(defun calc-pack ()
  (defun add (&rest x)
    (apply #'+ x)))

(defun add (x y)
  (+ x y ))

;;2-77
;;110ページから112ページでは
;;(put 'magnitude '(rectangular) ..
;;(put 'magnitude '(polar) ..
;; しか登録していない.
;;(put 'magnitude '(complex) ..
;; を追加しないと
;;complex rectangular 3 . 4)
;;のmagnitudeはとれない.
;;magnitude (complex rectangular 3.4))
;;(apply-generic magnitude (comples rectangular 3 . 4))
;;((get magnitude (complex)) (rectangular 3 . 4))
;;(magnitude (rectangular 3 . 4))
;;(apply-generic magnitude (rectangular 3 . 4))
;;((get magnitude (rectangular)) (3 . 4))
;;(magnitude (3 . 4))
;;5
;;という具合にapply-genericは2度呼び出される.)))
