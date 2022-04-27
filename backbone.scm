(load "logic.scm")
(load "utils.scm")

(define (define-rule
	  name
	  arity
	  procedure)
  #f)

(define (derive-recursive procedure
			  reversed-applicability
			  derived
			  sentences			  
			  args)
  (if (null? reversed-applicability)
      (apply procedure (cons derived args))
      (let ((possibilities (filter (lambda (sentence)
				     ((car reversed-applicability)
				      (get-sentence sentence)))
				   sentences)))
	(find-first (lambda (sentence)
		      (derive-recursive procedure
					(cdr reversed-applicability)
					derived
					sentences
					(cons sentence args)))
		    possibilities))))

(define (derive-general procedure
			applicability
			derived
			sentences)
  (derive-recursive procedure
		    (reverse applicability)
		    derived
		    sentences
		    (list)))
			
(define (try-apply procedure
		   applicability
		   derived
		   sentences)
  (and ((car applicability) derived)
       (derive-general procedure
		       (cdr applicability)
		       derived
		       sentences)))

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
  (define derived 'a)
  (define sentences (list (list '(implies b a)
			    '(p x))
		      (list 'c
			    '(q x))
		      (list 'd
			    '(r x))
		      (list '(implies c a)
			    '(s x))
		      (list '(implies a a)
			    '(t x))))
  
  (write (try-apply demorgans
		    (list true? demorgans?)
		    derived	    
		    sentences))
  (newline)
  ;; #f
	 
  (write (try-apply modus-ponens
		    (list true? implies? true?)
		    derived
		    sentences))
  (newline)
  ;; premise sentences - ((c (q x)) ((implies c a) (s x))
  ;; premise set - q s x)

)
