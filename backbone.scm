(load "logic.scm")

(define (define-rule
	  name
	  arity
	  procedure)
  #f)

(define (and2? exp)
  (and (list? exp)
       (= (length exp) 3)
       (eqv? (tag exp) 'and)))

(define (or2? exp)
  (and (list? exp)
       (= (length exp) 3)
       (eqv? (tag exp) 'or)))

(define (not1? exp)
  (and (list? exp)
       (= (length exp) 2)
       (eqv? (tag exp) 'not)))

(define (demorgans
	 derived
	 psi
	 psi-premise-set)
  (and
   (not1? derived)
   (and2? (cadr derived))
   (or2? psi)
   (not1? (second psi))
   (not1? (third psi))
   (let ((der1 (second (cadr derived)))
	 (der2 (third (cadr derived)))
	 (psi1 (cadr (second psi)))
	 (psi2 (cadr (third psi))))
     (and
      (equal? der1 psi1)
      (equal? der2 psi2)
      psi-premise-set))))

(and2? '(and a b))
(or2? '(or (not a) (not b)))
(not1? '(not a))
(not1? '(not b))

(demorgans
 '(not (and a b))
 '(or (not a) (not b))
 '(c d e))

(demorgans
 '(not (and (not a) (not b)))
 '(or a b)
 '(c d e))

(define (demorgans? psi)
  (and (or2? psi)
       (not1? (second psi))
       (not1? (third psi))))

(define (get-sentence sentence) (first sentence))
(define (get-premise-set sentence) (second sentence))

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
					(cons (get-sentence sentence)
					      (cons (get-premise-set sentence)
						    args))))
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
			

;;; Finds the first element of lis where pred is not #f
;;; Returns the element and the result of pred
(define (find-first pred lis)
  (if (not (list? lis)) (error:not-a list? lis 'find-first))
  (and (not (null? lis))
       (let ((res (pred (car lis))))
	 (if res
	     (cons (car lis) res)
	     (find-first pred (cdr lis))))))

(begin
  (define derived '(not (and a b)))
  (define alpha (cons '(not (and (not a) (not b)))
		      '(a w)))
  (define beta (cons '(or a b)
		     '(b x)))
  (define gamma (cons '(or (not a) (not b))
		      '(c y)))
  (define delta (cons '(not (and a b))
		      '(d z)))
  (define res (derive-general demorgans
			      (list demorgans?)
			      derived
			      (list alpha beta gamma)))
  (write res)
  ;(write (car res)) (newline)
  ;(write (cdr res)) (newline)
)


(begin
  (define derived '(implies  (and a b)))
  (define alpha (cons '(not (and (not a) (not b)))
		      '(a w)))
  (define beta (cons '(or a b)
		     '(b x)))
  (define gamma (cons '(or (not a) (not b))
		      '(c y)))
  (define delta (cons '(not (and a b))
		      '(d z)))
  (define res (derive-general demorgans
			      (list demorgans?)
			      derived
			      (list alpha beta gamma)))
  (write res)
  ;(write (car res)) (newline)
  ;(write (cdr res)) (newline)
  )
(define der '(implies ((atomic a) (constant s)) ((atomic b) (constant s))))
(define psi '((atomic b) (constant s)))
(define phi '((atomic a) (constant s)))
(define premise-set (list phi))


(define (modus-ponens
	 derived
	 phi
	 phi-premise-set
	 psi
	 psi-premise-set)
  (and
   (implies? phi)
   (equal? psi (second phi))
   (equal? derived (third phi))
   (lset-union equal? phi-premise-set psi-premise-set)))

(define true? (lambda (x) #t))

(define (implies? exp)
  (and (list? exp)
       (= (length exp) 3)
       (eqv? (tag exp) 'implies)))

(derive-general modus-ponens
		(list implies? true?)
		'a
		(list (list '(implies b a)
			    '(p x))
		      (list 'c
			    '(q x))
		      (list 'd
			    '(r x))
		      (list '(implies c a)
			    '(s x))
		      (list '(implies a a)
			    '(t x))))
;; premise sentences - ((c (q x)) ((implies c a) (s x))
;; premise set - q s x)
