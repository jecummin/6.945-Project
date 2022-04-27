;;(load "logic.scm")

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

(define (apply-demorgans
  derived
  sentences)
  (let ((possibilities (filter (lambda (sentence)
				 (demorgans? (car sentence)))
			       sentences)))
    (find-first (lambda (sentence)
		(demorgans derived (car sentence) (cdr sentence)))
		possibilities)))

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
  (define res (apply-demorgans derived (list alpha beta gamma)))
  (write (car res)) (newline)
  (write (cdr res)) (newline)
)
