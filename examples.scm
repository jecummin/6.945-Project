(load "logic")
;; (load "theology")



;; m -> is a man
;; d -> is mortal
;; s -> Socrates
(define socrates-is-mortal-argument
  (list '(universal (variable x) (implies ((atomic m) (variable x))
					  ((atomic d) (variable x))))
	'((atomic m) (constant s))
	'(implies ((atomic m) (constant s)) ((atomic d) (constant s)))
	'((atomic d) (constant s))))

(define result (analyze-argument socrates-is-mortal-argument))
(describe-argument result)
			     
