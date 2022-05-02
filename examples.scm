(load "backbone")
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

 ;; (define result (analyze-argument socrates-is-mortal-argument))
 ;; (describe-argument result)


;; f -> is a fish
;; w -> is warm-blooded
;; a -> is aquatic animal
(define not-all-aquatic-animals-are-fish
  (list '(lnot (existential (variable x)
			    (land ((atomic f) (variable x))
				  ((atomic w) (variable x)))))
	'(existential (variable x)
		      (land ((atomic a) (variable x))
			    ((atomic w) (variable x))))
	'(land ((atomic a) (constant s)) ((atomic w) (constant s)))
	'(universal (variable x)
		    (lnot (land ((atomic f) (variable x))
				((atomic w) (variable x)))))
	'(lnot (land ((atomic f) (constant s))
		     ((atomic w) (constant s))))
	'((atomic w) (constant s))
	'(lnot ((atomic f) (constant s)))
	'((atomic a) (constant s))
	'(land ((atomic a) (constant s)) ((atomic f) (constant s)))
	'(existential (variable x) (land ((atomic a) (variable x))
					 ((atomic f) (constant s))))
	'(existential (variable x) (land ((atomic a) (variable x))
					 ((atomic f) (constant s))))))
			     
(define result (analyze-argument not-all-aquatic-animals-are-fish))
(describe-argument result)
