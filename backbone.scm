(load "utils.scm")

#| INTERFACE |#

;;; Resets the list of rules
(define (reset-rules!)
  (set! rules (list)))

;;; Adds a rule
(define (define-rule!
	  name
	  procedure
	  applicability)
  (set! rules (cons (list name
			  procedure
			  applicability)
		    rules)))

;;; Analyzes an argument (list of sentences) using defined rules
(define (analyze-argument argument)
  (let lp ((argument argument)
	   (premises (list)))
    (if (null? argument)
	(reverse premises)
	(let ((step (try-apply-rules rules
				     (car argument)
				     premises)))
	  (lp (cdr argument)
	      (cons step premises))))))

;;; Prints the argument analysis in a more user-friendly format
;;; (The result of analyze-argument)
(define (describe-argument argument)
  (display "\nArgument:\n")
  (for-each describe-step argument))

;; Prints each step as it is generated
;; (Useful for debugging in case of an error)
(define (describe-and-analyze-argument argument)
  (let lp ((argument argument)
	   (premises (list)))
    (if (null? argument)
	(reverse premises)
	(let ((step (try-apply-rules rules
				     (car argument)
				     premises)))
	  (describe-step step)
	  (lp (cdr argument)
	      (cons step premises))))))

;;; Prints single step of an argument
(define (describe-step step)
  (display "Statement: ") (display (first step)) (newline)
	      (display "Premise Set: ") (newline)
	      (for-each (lambda (premise)
			  (display " - ")
			  (display premise)
			  (newline))
			(second step))
	      (display "Rule: ") (display (third step)) (newline)
	      (display "Premise Sentences: ") (newline)
	      (for-each (lambda (premise-sentence)
			  (display " - ")
			  (display premise-sentence)
			  (newline))
			(map car (reverse (fourth step))))
	      (newline))
    
    
  

#| BACKEND |#

(define rules (list))

;;; Returns false if rule is not applicable
;;; Otherwise returns pair
;;; car is the list of premise sentences
;;; cdr is the premise set
(define (derive-general procedure
			applicability
			derived
			sentences)
  (let lp ((procedure procedure)
	   (reversed-applicability (reverse applicability))
	   (derived derived)
	   (sentences sentences)
	   (args (list)))			   
    (if (null? reversed-applicability)
	(let ((res (apply procedure (cons derived args))))
	  (and res
	       (not (member (list derived res) sentences))
	       (cons (list) res)))
	(let ((possibilities (filter (lambda (sentence)
				       ((car reversed-applicability)
					(get-sentence sentence)))
				     sentences)))
	  (let ((res (find-first (lambda (sentence)
				   (lp procedure
				       (cdr reversed-applicability)
				       derived
				       sentences
				       (cons sentence args)))
				 possibilities)))
	    (if res
		(cons (cons (car res)
			    (car (cdr res)))
		      (cdr (cdr res)))
		#f))))))

;;; Returns false if rule is not applicable
;;; Otherwise returns pair
;;; car is the list of premise sentences
;;; cdr is the premise set
(define (try-apply procedure
		   applicability
		   derived
		   sentences)
  (and ((car applicability) derived)
       (derive-general procedure
		       (cdr applicability)
		       derived
		       sentences)))

;;; Returns a list of length 4:
;;; - derived sentence
;;; - premise set
;;; - rule name
;;; - premise sentences
;;; If no rules are applicable,
;;; The derived sentence is the premise for itself
;;; (And the rule name will be 'premise-induction)
(define (try-apply-rules rules
			 target
			 derivations)
  (let ((sentences-and-premises
	 (map (lambda (derivation)
		(list (derivation-sentence derivation)
		      (derivation-premise-set derivation)))
	      derivations)))
    (let ((inference
	   (find-first (lambda (rule)
			 (try-apply (rule-procedure rule)
				    (rule-applicability rule)
				    target
				    sentences-and-premises))
		       rules)))
      (if inference
	  (list target
		(inference-premise-set inference)
		(inference-rule-name inference)
		(inference-premise-sentences inference))
	  (list target
		(list target)
		'premise-induction
		(list (cons target
			    (list target))))))))

(begin
  (reset-rules!)
  (define-rule! 'modus-ponens
	        modus-ponens
	        (list true? implies? true?))
  (define-rule! 'demorgans
	        demorgans
	        (list true? demorgans?))
  
  (define alpha '(implies phi psi))
  (define beta '(implies psi upsilon))
  (define gamma 'phi)
  (define delta 'psi)
  (define epsilon 'upsilon)
  
  (define res (analyze-argument (list alpha
				      beta
				      gamma
				      delta
				      epsilon
				      epsilon)))

  (describe-argument res)
)

;;; Returns false if rule is not applicable
;;; Otherwise returns pair
;;; car is the list of premise sentences
;;; cdr is a list containing derived and premise set
(define (try-autoderive procedure
		        applicability
		        sentences)
  (let lp ((procedure procedure)
	   (reversed-applicability (reverse applicability))
	   (sentences sentences)
	   (args (list)))			   
    (if (null? reversed-applicability)
	(let ((derived (apply procedure args)))
	  (and derived
	       (not (member derived sentences))
	       (cons (list) derived)))
	(let ((possibilities (filter (lambda (sentence)
				       ((car reversed-applicability)
					(get-sentence sentence)))
				     sentences)))
	  (let ((res (find-first (lambda (sentence)
				   (lp procedure
				       (cdr reversed-applicability)
				       sentences
				       (cons sentence args)))
				 possibilities)))
	    (if res
		(cons (cons (car res)
			    (car (cdr res)))
		      (cdr (cdr res)))
		#f))))))

(define (autoderive-rules rules
			  ;target
			  derivations)
  (let ((sentences-and-premises
	 (map (lambda (derivation)
		(list (derivation-sentence derivation)
		      (derivation-premise-set derivation)))
	      derivations)))
    (let ((deduction
	   (find-first (lambda (rule)
			 (try-autoderive (rule-procedure rule)
				    (rule-applicability rule)
				    sentences-and-premises))
		       rules)))
      (if deduction
	  (list (second (cdr deduction))
		(third (cdr deduction))
		(rule-name (car deduction))
		(first (cdr deduction)))
	  #f))))

(define (create-premise premise)
  (list premise
	(list premise)
	'premise-induction
	(list (cons premise
		    (list premise)))))

(define (autoderive-argument rules
			     premises)
  (let lp ((derivations (map create-premise
			     premises)))
    (let ((step (autoderive-rules rules
				  derivations)))
      (if step
	  (lp (append derivations (list step)))
	  derivations))))

(define (derive-modus-ponens phi-and-premise-set
			     psi-and-premise-set)
  (define phi (get-sentence phi-and-premise-set))
  (define phi-premise-set (get-premise-set phi-and-premise-set))
  (define psi (get-sentence psi-and-premise-set))
  (define psi-premise-set (get-premise-set psi-and-premise-set))
  (and
   (implies? phi)
   (equal? psi (second phi))
   (let ((derived (third phi))
	 (premise-set (lset-union equal?
				  phi-premise-set
				  psi-premise-set)))
     (list derived premise-set))))

(define modus-ponens-rule (list 'modus-ponens
			        derive-modus-ponens
				(list implies? true?)))

(let ((step (autoderive-rules (list modus-ponens-rule)
	    '(((implies phi psi)
	       ((implies phi psi))
	       premise-induction
	       ((implies phi psi)))
	      (phi
	       (phi)
	       premise-induction
	       (phi))))))
  (describe-step step))

(autoderive-argument (list modus-ponens-rule)
	    '(((implies phi psi)
	       ((implies phi psi))
	       premise-induction
	       ((implies phi psi)))
	      (phi
	       (phi)
	       premise-induction
	       (phi))))

(let ((arg (autoderive-argument (list modus-ponens-rule)
				(list '(implies phi psi)
				      '(implies psi tau)
				      'phi))))
  
  (describe-argument arg))
