(load "utils.scm")

#| USER INTERFACE |#

;;; Resets the list of rules
(define (reset-rules!)
  (set! rules (list)))

;;; Adds a rule into the specified rule group
;;; Default is the "core" rule group
(define (define-rule! name procedure applicability #!optional group)
  (let ((group (if (equal? #!default group)
		   'core
		   group)))
    (let ((rule (make-rule name procedure applicability group)))
      (set! rules (cons rule rules)))))


;;; Get the list of rules
(define (get-rules) rules)

;;; List each rule's group, id, name
(define (describe-rules)
  (for-each describe-rule rules))

(define (describe-rule rule)
  (display-line (rule-group rule)
                " (ID " (rule-id rule) ") "
		(rule-name rule)))

(define (get-by-group . groups)
  (let ((s (split rules (lambda (rule)
			  (member (rule-group rule) groups)))))
    (car s)))

;;; Removes all rules from the ruleset with a given name
;;; If multiple names given, remove each name
;;; If name does not exist, no change occurs
;;; Returns list of removed rules
(define (delete-rule-by-name . names)
  (let ((s (split rules (lambda (rule)
			  (member (rule-name rule) names)))))
    (set! rules (cdr s))
    (car s)))
			  
;;; Removes a rule from the ruleset given its id
;;; If multiple ids given, deletes each id
;;; If id does not exist, no change occurs
;;; Returns list of removed rules
(define (delete-rule-by-id . ids)
  (let ((s (split rules (lambda (rule)
			  (member (rule-id rule) id)))))
    (set! rules (cdr s))
    (car s)))
	
;;; Analyzes an argument (list of sentences) using all defined rules
(define (analyze-argument argument . groups)		   
  (let lp ((argument argument)
	   (premises (list)))
    (if (null? argument)
	(reverse premises)
	(let ((step (try-apply-rules rules
				     (car argument)
				     premises)))
	  (lp (cdr argument)
	      (cons step premises))))))

;;; Analyzes an argument (list of sentences) using specific rule sets
;;; Note: "core" must be specified if desired (not automatically included)
(define (analyze-argument-with argument . groups)
  (let ((rules (apply get-by-group groups)))
    (let lp ((argument argument)
	     (premises (list)))  
      (if (null? argument)	  
	  (reverse premises)
	  (let ((step (try-apply-rules rules
				       (car argument)
				       premises)))
	    (lp (cdr argument)
		(cons step premises)))))))
  
;;; Prints the argument analysis in a more user-friendly format
;;; (The result of analyze-argument)
(define (describe-argument argument)
  (display "\nArgument:\n")
  (for-each describe-step argument))

;;; Prints single step of an argument
(define (describe-step step)
  (display-line "Statement: " (step-sentence step))
  (display-line "Premise Set: ")
  (for-each (lambda (premise)
	      (display-line " - " premise))  
	    (step-premise-set step))
  (display-line "Rule: " (step-rule step))
  (display-line "Premise Sentences: ")
  (for-each (lambda (premise-sentence)
	      (display-line " - " premise-sentence))
	    (map car (reverse (step-premise-sentences step))))
  (newline))

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

    
#| CORE (BACKEND) |#

(define rules (list))

(define rule-id-counter 0)
(define (new-id) (set! rule-id-counter (+ rule-id-counter 1)))

(define (make-rule name procedure applicability group)
  (list 'rule name procedure applicability (new-id) group))

(define (make-step sentence premise-set rule premise-sentences)
  (list 'step sentence premise-set rule premise-sentences))

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
		(list (step-sentence derivation)
		      (step-premise-set derivation)))
	      derivations)))
    (let ((inference
	   (find-first (lambda (rule)
			 (try-apply (rule-procedure rule)
				    (rule-applicability rule)
				    target
				    sentences-and-premises))
		       rules)))
      (if inference
	  (make-step
	        target
		(inference-premise-set inference)
		(inference-rule-name inference)
		(inference-premise-sentences inference))
	  (make-step
	        target
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

#|
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
|#
