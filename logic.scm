(load "backbone.scm")

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

(define (land? x)
  (eqv? (tag x) 'land))

(define (lor? x)
  (eqv? (tag x) 'lor))

(define (not? x)
  (eqv? (tag x) 'lnot))

(define (implies? x)
  (eqv? (tag x) 'implies))

(define (iff? x)
  (eqv? (tag x) 'iff))

(define (connective? x)
  (or (land? x) (lor? x) (not? x) (implies? x) (iff? x)))

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
   ((not? sent) (get-constants (first-argument sent)))
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



#|
rule schema:

generic procedure
takes 
   - argument name
   - initial applicability of said sentences (1st is derived sentence,
     2nd are possible dependencies)
   - procedure that takes sentences and premise set and returns new

premise set if derivation applies, #f otherwise
|#

;; conditional-proof-applicability derived phi chi
;; (implies? derived)
;; (sentence? phi)
;; (sentence? psi)
(define (conditional-proof-rule derived phi-and-premises psi-and-premises)
  (let ((phi (car phi-and-premises))
	(phi-premise-set (cadr phi-and-premises))
	(psi (car psi-and-premises))
	(psi-premise-set (cadr psi-and-premises)))
  (if (and (sentence-eqv? (first-argument derived) phi)
	   (sentence-eqv? (second-argument derived) psi))
      (delete phi psi-premise-set)
      #f)))

(define-rule!
  'conditional-proof
  conditional-proof-rule
  (list implies? sentence? sentence?))

#|
(define der '(implies ((atomic a) (constant s)) ((atomic b) (constant s))))
(define psi '((atomic b) (constant s)))
(define phi '((atomic a) (constant s)))
(define premise-set (list phi))

(conditional-proof-rule der (list phi '()) (list psi premise-set))
;Value: ()

(define xi '((atomic a) (constant b)))
(conditional-proof-rule der (list xi '()) (list psi premise-set))
;Value: #f				;
|#

;; universal-specification-applicability derived universal
;; (sentence? derived)
;; (universal? universal)
(define (universal-specification-rule derived universal-and-premises)
  (let ((universal (car universal-and-premises))
	(premise-set (cadr universal-and-premises)))
  (if (is-substitution-instance?
       derived
       (second-argument universal)
       (first-argument universal))
      premise-set
      #f)))

(define-rule!
  'universal-specification
  universal-specification-rule
  (list sentence? universal?))

#|
(define der '((atomic a) (constant s)))
(define uni '(universal (variable x) ((atomic a) (variable x))))
(define premise-set '(((atomic b) (constant t))))

(universal-specification-rule der (list uni premise-set))
;Value: (((atomic b) (constant t)))

(universal-specification-rule '((atomic b) (constant t)) (list uni premise-set))
;Value: #f


(define u '(universal (variable x)
		    (lnot (land ((atomic f) (variable x))
				((atomic w) (variable x))))))
(define d	'(lnot (land ((atomic f) (constant s))
		     ((atomic w) (constant s)))))
(universal-specification-rule d (list u '()))
|#

;; universal-generalization-applicability derived spec
;; (universal? derived)
;; (sentence? spec)

(define (all bool-list)
  (reduce (lambda (x y) (and  x y)) #t bool-list))

(define (none bool-list)
  (not (any bool-list)))

(define (any bool-list)
  (reduce (lambda (x y) (or  x y)) #f bool-list))

(define (universal-generalization-rule derived spec-and-premises)
  (let ((spec (car spec-and-premises))
	(premise-set (cadr spec-and-premises)))
  (let ((const (is-substitution-instance?
		spec
		(second-argument derived)
		(first-argument derived))))
    (if const
	(if (none (map (lambda (x) (contains-term? x
							      const)) premise-set))
	    premise-set
	    #f)
	#f))))


(define-rule!
  'universal-generalization
  universal-generalization-rule
  (list universal? sentence?))

#|
(define spec '((atomic a) (constant s)))
(define der '(universal (variable x) ((atomic a) (variable x))))
(define premise-set1 '(((atomic b) (constant t))))
(define premise-set2 '(((atomic b) (constant s))))

(universal-generalization-rule der (list spec premise-set1))
;Value: (((atomic b) (constant t)))

(universal-generalization-rule der (list spec premise-set2))
;Value: #f
|#



#|
;; universal-specification-applicability derived sentence
both args must be qe-applicable
|#

(define (qe-applicable? x) 
  (or 
   (or (universal? x) 
       (and (not? x) (universal? (first-argument x))))
   (or (existential? x) 
       (and (not? x) (existential? (first-argument x))))))

(define (negated? x y)
    (and (not? y)
	 (sentence-eqv? x (first-argument y))))

(define (quantifier-exchange-rule derived sentence-and-premises)
  (let ((sentence (car sentence-and-premises))
 	(premise-set (cadr sentence-and-premises)))

  (cond
   ((universal? derived)
    (if (and (not? sentence) (existential? (first-argument sentence)))
	(cond
	 ((negated? (second-argument derived)
		    (second-argument (first-argument sentence)))  premise-set)
	 ((negated? (second-argument (first-argument sentence))
		    (second-argument derived))  premise-set)
	 (else #f))
	#f))
   ((and (not? derived) (universal? (first-argument derived)))
    (if (existential? sentence)
	(cond
	 ((negated? (second-argument (first-argument derived))
		    (second-argument sentence))  premise-set)
	 ((negated? (second-argument sentence)
		    (second-argument (first-argument derived)))  premise-set)
	 (else #f))
	#f))
   ((existential? derived)
    (if (and (not? sentence) (universal? (first-argument sentence)))
	(cond
	 ((negated? (second-argument derived)
		    (second-argument (first-argument sentence)))  premise-set)
	 ((negated? (second-argument (first-argument sentence))
		    (second-argument derived))  premise-set)
	 (else #f))
	#f))
   ((and (not? derived) (existential? (first-argument derived)))
        (if (universal? sentence)
	(cond
	 ((negated? (second-argument (first-argument derived))
		    (second-argument sentence))  premise-set)
	 ((negated? (second-argument sentence)
		    (second-argument (first-argument derived)))  premise-set)
	 (else #f))
	#f))
   (else #f))))

(define-rule!
  'quantifier-exchange
  quantifier-exchange-rule
  (list qe-applicable? qe-applicable?))


#|
(define premise-set '(this-is-a-premise-set))

(define der1 '(universal (variable x) ((atomic A) (variable x))))
(define prem1 '(lnot (existential (variable x) (lnot ((atomic A) (variable x))))))

(define der2 '(lnot (universal (variable x) ((atomic A) (variable x)))))
(define prem2 '(existential (variable x) (lnot ((atomic A) (variable x)))))

(define der3 '(universal (variable x) (lnot ((atomic A) (variable x)))))
(define prem3 '(lnot (existential (variable x) ((atomic A) (variable x)))))

(define der4 '(lnot (universal (variable x) (lnot ((atomic A) (variable x))))))
(define prem4 '(existential (variable x) ((atomic A) (variable x))))

(quantifier-exchange-rule der1 (list prem1 premise-set))
(quantifier-exchange-rule prem1 (list der1 premise-set))
(quantifier-exchange-rule der2 (list prem2 premise-set))
(quantifier-exchange-rule prem2 (list der2 premise-set))
(quantifier-exchange-rule der3 (list prem3 premise-set))
(quantifier-exchange-rule prem3 (list der3 premise-set))
(quantifier-exchange-rule der4 (list prem4 premise-set))
(quantifier-exchange-rule prem4 (list der4 premise-set))
|#


(define (tc-rule-1 derived conj-and-premises)
  (let ((conj (car conj-and-premises))
	(premise-set (cadr conj-and-premises)))	      
    (if (or (sentence-eqv? derived (first-argument conj))
	    (sentence-eqv? derived (second-argument conj)))
	premise-set
	#f)))

(define-rule!
  'tautological-consequence
  tc-rule-1
  (list sentence? land?))

#|
(define der '((atomic a) (constant s)))
(define conj1 '(and ((atomic b) (constant s)) ((atomic a) (constant s))))
(define conj2 '(and ((atomic a) (constant s)) ((atomic b) (constant s))))

(tc-rule-1 der (list conj1 '()))
(tc-rule-1 der (list conj2 '()))
|#

(define (negated-pair? x y)
  (or (negated? x y) (negated? y x)))

(define (tc-rule-2 derived disj-and-premises sent-and-premises)
  (let ((disj (car disj-and-premises))
	(disj-premise-set (cadr disj-and-premises))
	(sent (car sent-and-premises))
	(sent-premise-set (cadr sent-and-premises)))
    (cond
     ((sentence-eqv? derived (first-argument disj))
      (if (negated-pair? sent (second-argument disj))
	  (lset-union disj-premise-set sent-premise-set)
	  #f))
     ((sentence-eqv? derived (second-argument disj))
      (if (negated-pair? sent (first-argument disj))
	  (lset-union disj-premise-set sent-premise-set)
	  #f))
     (else #f))))

(define-rule!
  'tautological-consequence
  tc-rule-2
  (list sentence? lor? sentence?))

(define (tc-rule-3 derived imp-and-premises sent-and-premises)
  (let ((imp (car imp-and-premises))
	(imp-premise-set (cadr imp-and-premises))
	(sent (car sent-and-premises))
	(sent-premise-set (cadr sent-and-premises)))
    (if (sentence-eqv? derived (second-argument imp))
	(if (sentence-eqv? sent (first-argument imp))
	    (lset-union imp-premise-set sent-premise-set)
	    #f)
	#f)))

(define-rule!
  'tautological-consequence
  tc-rule-3
  (list sentence? implies? sentence?))

#|
(define der '((atomic a) (constant s)))
(define imp '(implies ((atomic b) (constant s)) ((atomic a) (constant s))))
(define sent '((atomic b) (constant s)))

(tc-rule-3 der (list imp '()) (list sent '()))
|#

(define (tc-rule-4 derived nconj-and-premises sent-and-premises)
  (let ((conj (first-argument (car nconj-and-premises)))
	(conj-premise-set (cadr nconj-and-premises))
	(sent (car sent-and-premises))
	(sent-premise-set (cadr sent-and-premises)))
    (cond
     ((sentence-eqv? sent (first-argument conj))
      (if (negated-pair? derived (second-argument conj))
	  (lset-union conj-premise-set sent-premise-set)
	  #f))
     ((sentence-eqv? sent (second-argument conj))
      (if (negated-pair? derived (first-argument conj))
	  (lset-union conj-premise-set sent-premise-set)
	  #f))
     (else #f))))

(define-rule!
  'tautological-consequence
  tc-rule-4
  (list sentence? (lambda (x) (and (not? x) (land? (first-argument x)))) sentence?))

#|
(define der '(lnot ((atomic f) (constant s))))
(define nconj 	 '(lnot (land ((atomic f) (constant s)) ((atomic w) (constant s)))))
(define sent 	 '((atomic w) (constant s)))

(tc-rule-4 der (list nconj '()) (list sent '()))
|#

(define (tc-rule-5 derived sent1-and-premises sent2-and-premises)
  (let ((sent1 (car sent1-and-premises))
	(sent1-premise-set (cadr sent1-and-premises))
	(sent2 (car sent2-and-premises))
	(sent2-premise-set (cadr sent2-and-premises)))
    (if (or
	 (and (sentence-eqv? (first-argument derived) sent1)
	      (sentence-eqv? (second-argument derived) sent2))
	 (and (sentence-eqv? (first-argument derived) sent2)
	      (sentence-eqv? (second-argument derived) sent1)))
	(lset-union sent1-premise-set sent2-premise-set)
	#f)))

#|

|#

(define-rule!
  'tautological-consequence
  tc-rule-5
  (list land? sentence? sentence?))


;; existential-generalization-applicability derived spec
;; (existential? derived)
;; (sentence? spec)
(define (existential-generalization-rule derived spec-and-premises)
  (let ((spec (car spec-and-premises))
	(premise-set (cadr spec-and-premises)))
  (if (is-substitution-instance?
       spec
       (second-argument derived)
       (first-argument derived))
      premise-set
      #f)))

(define-rule!
  'existential-generalization
  existential-generalization-rule
  (list existential? sentence?))

#|
(define spec1 '((atomic a) (constant s)))
(define spec2 '((atomic b) (constant s)))
(define der '(existential (variable x) ((atomic a) (variable x))))
(define premise-set '(((atomic b) (constant t))))

(existential-generalization-rule der (list spec1 premise-set))
;Value: (((atomic b) (constant t)))

(existential-generalization-rule der (list spec2 premise-set))
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
					existential-and-premises
					psi-and-premises)
  (let ((existential (car existential-and-premises))
	(existential-premise-set (cadr existential-and-premises))
	(psi (car psi-and-premises))
	(psi-premise-set (cadr psi-and-premises)))
  (if (not (equal? derived psi))
      #f
      (let ((subs (which (map (lambda (x) (is-substitution-instance?
				    x
				    (second-argument existential)
				    (first-argument existential)))
		       psi-premise-set))))
	(cond
	 ((= (length subs) 0) #f)
	 ((> (length subs) 1) #f)
	 (else (let ((const (is-substitution-instance?
			      (list-ref  psi-premise-set (car subs))
			      (second-argument existential)
			      (first-argument existential)))
		     (lmbda (delete-i psi-premise-set (car subs))))
		  (if (none (map (lambda (x) (contains-term? x const)) lmbda))
		      (append existential-premise-set lmbda)
		      #f))))))))


(define-rule!
  'existential-specification
  existential-specification-rule
  (list sentence? existential? sentence?))


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
