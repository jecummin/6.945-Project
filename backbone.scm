(load "utils.scm")

#| INTERFACE |#

; Resets the list of rules
(define (reset-rules!)
  (set! rules (list)))

; Adds a rule
(define (define-rule!
	  name
	  procedure
	  applicability)
  (set! rules (cons (list name
			  procedure
			  applicability)
		    rules)))

; Analyzes an argument (list of sentences) using defined rules
(define (analyze-argument argument)
  (reverse (analyze-argument-recursive argument
				       (list))))

; Prints the argument in a more user-friendly format
(define (describe-argument argument)
  (display "\nArgument:\n")
  (for-each (lambda (step)
	      (display "Statement: ") (display (first step)) (newline)
	      (display "Premise Set: ") (display (second step)) (newline)
	      (display "Rule: ") (display (third step)) (newline)
	      (display "Premise Sentences: ")
	      (display (map car (fourth step))) (newline)
	      (newline))
	    argument))

#| BACKEND |#

(define rules (list))

;;; Returns false if rule is not applicable
;;; Otherwise returns pair
;;; car is the list of premise sentences
;;; cdr is the premise set
(define (derive-recursive procedure
			  reversed-applicability
			  derived
			  sentences			  
			  args)
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
				 (derive-recursive procedure
						   (cdr reversed-applicability)
						   derived
						   sentences
						   (cons sentence args)))
			       possibilities)))
	  (if res
	      (cons (cons (car res)
			  (car (cdr res)))
		    (cdr (cdr res)))
	      #f)))))


;;; Returns false if rule is not applicable
;;; Otherwise returns pair
;;; car is the list of premise sentences
;;; cdr is the premise set
(define (derive-general procedure
			applicability
			derived
			sentences)
  (derive-recursive procedure
		    (reverse applicability)
		    derived
		    sentences
		    (list)))


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

;;; Returns false if no rules are applicable
;;; Otherwise returns a list of length 4:
;;; - derived sentence
;;; - premise set
;;; - rule name
;;; - premise sentences
(define (try-apply-rules rules
			 derived
			 sentences)
  (let ((premises (find-first
		   (lambda (rule)
		     (try-apply (second rule)
				(third rule)
				derived
				(map (lambda (sentence)
				       (cons (first sentence)
					     (second sentence)))
				     sentences)))
		   rules)))
    (if premises
	(list derived
	      (cdr (cdr premises))
	      (first (car premises))
	      (car (cdr premises)))
	(list derived
	      (list derived)
	      'premise-induction
	      (list (cons derived
			  (list derived)))))))

(define (analyze-argument-recursive argument
				    premise-sentences)
  (if (null? argument)
      premise-sentences
      (let ((step (try-apply-rules rules
				   (first argument)
				   premise-sentences)))
	(analyze-argument-recursive (cdr argument)
				    (cons step premise-sentences)))))
  
(begin
  (define derived '(not (and a b)))
  (define alpha (list '(not (and (not a) (not b)))
		      '(a w)))
  (define beta (list '(or a b)
		     '(b x)))
  (define gamma (list '(or (not a) (not b))
		      '(c y)))
  (define delta (list '(not (and a b))
		      '(d z)))
  (define res (derive-general demorgans
			      (list demorgans?)
			      derived
			      (list alpha beta gamma)))
  (write (car res)) (newline)
  (write (cdr res)) (newline)
)

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
  
