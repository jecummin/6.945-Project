;; (load "~/workspace/6945/sdf/manager/load")
;; (manage 'new 'user-defined-types)


(define (tag x)
  (car x))

(define (arguments x)
  (cdr x))

(define (first-argument x)
  (car (arguments x)))

(define (second-argument x)
  (cadr (arguments x)))

(define (universal? x) ; first argument is variable, second is formula
  (eqv? (tag x) 'universal))

(define (existential? x) ; first argument is variable, second is formula
  (eqv? (tag x) 'existential))

(define (variable? x)
  (eqv? (tag x) 'variable))

(define (constant? x)
  (eqv? (tag x) 'constant))

(define (atomic? x)
  (eqv? (tag x) 'atomic)) ; formula or sentence depending on whether x
					; or constant
(define (atomic-formula? x)
  (and
   (pair? (car x))
   (atomic? (car x))
   (variable? (first-argument x))))

(define (atomic-sentence? x)
  (and
   (pair? (car x))
   (atomic? (car x))
   (constant? (first-argument x))))

(define (and? x)
  (eqv? (tag x) 'land))

(define (or? x)
  (eqv? (tag x) 'lor))

(define (not? x)
  (eqv? (tag x) 'lnot))

(define (implies? x)
  (eqv? (tag x) 'implies))

(define (iff? x)
  (eqv? (tag x) 'iff))

(define (connective? x)
  (or (and? x) (or? x) (not? x) (implies? x) (iff? x)))

(define (binary-connective? x)
  (and (connective? x) (not (not? x))))

(define (nonquantifier-sentence? x)
  (cond
   ((binary-connective? x) (and
			    (nonquantifier-sentence? (first-argument x))
			    (nonquantifier-sentence? (second-argument x))))
   ((not? x) (nonquantifier-sentence? (first-argument x)))
   ((atomic-sentence? x) #t)
   (else #f)))

(define (sentence? x)
  (or (universal? x) (existential? x) (atomic-sentence? x)
      (nonquantifier-sentence? x)))

(define (formula? x)
  (not (sentence? x)))

#|
(atomic-sentence? '((atomic A) (constant a))) ;-> #t
(universal? '(universal (atomic A) (variable x))) ;-> #t
(sentence? '(lnot 
	     (land ((atomic A) (constant a))  
		   (implies ((atomic A)
			     (constant b))
			    ((atomic B)
			     (constant b)))))) ;-> #t
     -(Aa /\ (Ab -> Bb))
|#


(define (substitution-instance sent const var)
    (cond
     ((universal? sent)
      (list 'universal (first-argument sent)
	    (substitution-instance
	     (second-argument sent)
	     const var)))
     ((existential? sent)
      (list 'existential (first-argument sent)
	    (substitution-instance
	     (second-argument sent)
	     const var)))
     ((binary-connective? sent) (append (list (tag sent))
					   (append (list (substitution-instance
					    (first-argument sent)
					    const var))
					(list (substitution-instance
					  (second-argument sent) const var)))))
     ((not? sent)  (append (list (tag sent)) (list (substitution-instance
						 (first-argument sent)
						 const var))))
     ((atomic-sentence? sent) sent)
     ((atomic-formula? sent)
      (if (equal? (first-argument sent) var)
	  (list (car sent) const)
	  sent))
     (else (error "Unknown sentence type"))))

#|
(define form1 '(land ((atomic a) (variable x)) (lnot ((atomic b) (variable x)))))

(substitution-instance form1 '(constant s) '(variable x))
;Value: (land ((atomic a) (constant s)) (lnot ((atomic b) (constant s))))


(define form2 '(land ((atomic a) (variable x)) (lnot ((atomic b) (variable y)))))
(substitution-instance form2 '(constant s) '(variable x))
;Value: (land ((atomic a) (constant s)) (lnot (atomic b) (variabley)))

(define form3 '(universal (variable x) (land ((atomic a) (variable x))(lnot ((atomic b) (variable y))))))
(substitution-instance form3 '(constant s) '(variable y))
;Value: (universal (variable x) (land ((atomic a) (variable x)) (lnot ((atomic b) (constant s)))))
|#

(define (contains-term? sent term)
  (cond
   ((universal? sent) (contains-term? (second-argument sent) term))
   ((existential? sent) (contains-term? (second-argument sent) term))
   ((binary-connective? sent) (or (contains-term? (first-argument
						   sent) term)
				  (contains-term? (second-argument
						   sent) term)))
   ((not? sent) (contains-term? (first-argument sent) term))
   ((atomic-sentence? sent) (equal? (first-argument sent) term))
   ((atomic-formula? sent) (equal? (first-argument sent) term))))

#|
(contains-term? form1 '(variable x))
;Value: #t

(contains-term? form3 '(variable x))
;Value: #t

(contains-term? form3 '(constant s))
;Value: #f
|#

(define (get-constants sent)
  (cond
  ((universal? sent) (get-constants (second-argument sent)))
   ((existential? sent) (get-constants (second-argument sent)))
   ((binary-connective? sent) (append (get-constants (first-argument
						   sent))
				  (get-constants (second-argument sent))))
   ((not? sent) (get-constants sent))
   ((atomic-sentence? sent) (list (first-argument sent)))
   ((atomic-formula? sent) '())
   (else (error "Unknown sentence type"))))

#|
(define sent1 '(lor ((atomic b) (constant s)) (implies ((atomic a)
						       (constant g))
						      ((atomic p)
						       (constant w)))))
(get-constants sent1)
;Value: ((constant s) (constant g) (constant w))

(define sent2 '(existential (variable x) (land ((atomic a) (variable x)) ((atomic b) (constant s)))))
(get-constants sent2)
;Value: ((constant s))

|#

(define sentence-eqv? equal?)

(define (is-substitution-instance? sub sent var)
  (let lp ((constants (get-constants sub)))
    (if (null? constants)
	#f
	(if (sentence-eqv? (substitution-instance sent (car constants)
						  var) sub)
	    (car constants)
	    (lp (cdr constants))))))

#|
(define sent '(land ((atomic a) (variable x)) ((atomic b) (variable y))))
(define sub1 '(land ((atomic a) (variable x)) ((atomic b) (constant s))))


(is-substitution-instance? sub1 sent '(variable y))
;Value: (constant s)

(is-substitution-instance? sub1 sent '(variable x))
;Value: #f

|#


;;;(define (premise-induction premise-set)) ?????


#|
rule schema:

generic procedure
takes 
   - number of sentences to look at
   - initial applicability of said sentences (1st is derived sentence,
     2nd are possible dependencies)
   - procedure that takes sentences and premise set and returns new

premise set if derivation applies, #f otherwise
|#

;; conditional-proof-applicability derived phi chi
;; (implies? derived)
;; (sentence? phi)
;; (sentence? psi)
(define (conditional-proof-rule derived phi psi phi-premise-set psi-premise-set)
  (if (and (sentence-eqv? (first-argument derived) phi)
	   (sentence-eqv? (second-argument derived) psi))
      (delete phi phi-premise-set)
      #f))

#|
(define der '(implies ((atomic a) (constant s)) ((atomic b) (constant s))))
(define psi '((atomic b) (constant s)))
(define phi '((atomic a) (constant s)))
(define premise-set (list phi))

(conditional-proof-rule der phi psi premise-set '())
;Value: ()

(define xi '((atomic a) (constant b)))
(conditional-proof-rule der xi psi premise-set '())
;Value: #f				;
|#

;; universal-specification-applicability derived universal
;; (sentence? derived)
;; (universal? universal)
(define (universal-specification-rule derived universal premise-set)
  (if (is-substitution-instance?
       derived
       (second-argument universal)
       (first-argument universal))
      premise-set
      #f))

#|
(define der '((atomic a) (constant s)))
(define uni '(universal (variable x) ((atomic a) (variable x))))
(define premise-set '(((atomic b) (constant t))))

(universal-specification-rule der uni premise-set)
;Value: (((atomic b) (constant t)))

(universal-specification-rule '((atomic b) (constant t)) uni premise-set)
;Value: #f

|#

;; universal-generalization-applicability derived spec
;; (universal? derived)
;; (sentence? spec)

(define (all bool-list)
  (reduce (lambda (x y) (and  x y)) #t bool-list))

(define (none bool-list)
  (not (any bool-list)))

(define (any bool-list)
  (reduce (lambda (x y) (or  x y)) #t bool-list))

(define (universal-generalization-rule derived spec premise-set)
  (let ((const (is-substitution-instance?
		spec
		(second-argument derived)
		(first-argument derived))))
    (if const
	(if (none (map (lambda (x) (contains-term? x
							      const)) premise-set))
	    premise-set
	    #f)
	#f)))


#|
(define spec '((atomic a) (constant s)))
(define der '(universal (variable x) ((atomic a) (variable x))))
(define premise-set1 '(((atomic b) (constant t))))
(define premise-set2 '(((atomic b) (constant s))))

(universal-generalization-rule der spec premise-set1)
;Value: (((atomic b) (constant t)))

(universal-generalization-rule der  spec premise-set2)
;Value: #f
|#




;;; QE ugh I'll do it later

#|
;; universal-specification-applicability derived sentence
 (define (qe-applicable? x) 
   (or 
    (or (universal? x) 
	(and (negation? x) (universal? (first-argument x))))
    (or (existential? x) 
	(and (negation? x) (existential? (first-argument x))))))

both args must be qe-applicable

|#
(define (quantifier-exchange-rule derived sentence premise-set)
  (define (negated? x y)
    (and (negation? y)
    (sentence-eqv? x (first-argument y))))
  (cond
   ((universal? x)
    (if (and (negation? y) (existential? (first-argument y)))
	(cond
	 ((negated? x y)  premise-set)
	 ((negated? y x)  premise-set)
	 (else #f))
	#f))
   ((and (negation? x) (universal? (first-argument x)))
    (if (existential? y)
	(cond
	 ((negated? x y)  premise-set)
	 ((negated? y x)  premise-set)
	 (else #f))
	#f))
   ((existential? x)
    (if (and (negation? y) (universal? (first-argument y)))
	(cond
	 ((negated? x y)  premise-set)
	 ((negated? y x)  premise-set)
	 (else #f))
	#f))
   ((and (negation? x) (existential? (first-argument x)))
        (if (universal? y)
	(cond
	 ((negated? x y)  premise-set)
	 ((negated? y x)  premise-set)
	 (else #f))
	#f))
   (else #f)))


;;; TC ugh I'll do it later




;; existential-generalization-applicability derived spec
;; (existential? derived)
;; (sentence? spec)
(define (existential-generalization-rule derived spec premise-set)
  (if (is-substitution-instance?
       spec
       (second-argument derived)
       (first-argument derived))
      premise-set
      #f))

#|
(define spec1 '((atomic a) (constant s)))
(define spec2 '((atomic b) (constant s)))
(define der '(existential (variable x) ((atomic a) (variable x))))
(define premise-set '(((atomic b) (constant t))))

(existential-generalization-rule der spec1 premise-set)
;Value: (((atomic b) (constant t)))

(existential-generalization-rule der spec2 premise-set)
;Value: #f
|#


;; existential-specification-applicability derived existential psi
;; (sentence? derived)
;; (existential? existential)
;; (sentence? psi)

(define (which bool-list)
  (let lp ((bl bool-list) (idx 0) (return-list '()))
    (if (null? bl)
	return-list
	(if (car bl)
	    (lp (cdr bl) (+ 1 idx) (append return-list (list idx)))
	    (lp (cdr bl) (+ 1 idx) return-list)))))

(define (delete-i list i)
  (if (= i 0)
      (cdr list)
      (cons (car list) (delete-i (cdr list) (- i 1)))))


(define (existential-specification-rule derived
					existential
					psi
					existential-premise-set
					psi-premise-set)
  (if (not (equal? derived psi))
      #f
      (let ((subs (which (map (lambda (x) (is-substitution-instance?
				    x
				    (second-argument existential)
				    (first-argument existential)))
		       psi-premise-set))))
	(write-line subs)
	(cond
	 ((= (length subs) 0) #f)
	 ((> (length subs) 1) #f)
	 (else (let ((const (is-substitution-instance?
			      (list-ref  psi-premise-set (car subs))
			      (second-argument existential)
			      (first-argument existential)))
		     (lmbda (delete-i psi-premise-set (car subs))))
		 (write const)
		 (write (map (lambda (x) (contains-term? x const)) lmbda))
		  (if (none (map (lambda (x) (contains-term? x const)) lmbda))
		      (append existential-premise-set lmbda)
		      #f)))))))


#|
(define psi '((atomic a) (constant s)))
(define existential '(existential (variable x) ((atomic b) (variable x))))
(define sub '((atomic b) (constant s)))
(define psi-premise-set-1 '(((atomic a) (constant s)) ((atomic b) (constant t))))
(define psi-premise-set-2 '(((atomic a) (constant s)) ((atomic b) (constant s))))
(define existential-premise-set '(universal (variable x) ((atomic c) (variable x))))

(existential-specification-rule psi existential psi existential-premise-set psi-premise-set-1)
;Value: (((atomic b) (constant t)))

(existential-specification-rule '((atomic a) (constant t)) existential psi existential-premise-set psi-premise-set-1)
;Value: #f

(existential-specification-rule psi existential psi existential-premise-set psi-premise-set-2)
;Value: #f
|#





