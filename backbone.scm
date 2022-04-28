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
  (for-each (lambda (step)
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
	    argument))

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
	  (if res
	      (cons (list) res)
	      #f))
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
				      epsilon)))

  (describe-argument res)
)
  
