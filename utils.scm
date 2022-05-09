;;; Finds the first element of lis where pred is not #f
;;; Returns the element and the result of pred
(define (find-first pred lis)
  (if (not (list? lis)) (error:not-a list? lis 'find-first))
  (and (not (null? lis))
       (let ((res (pred (car lis))))
	 (if res
	     (cons (car lis) res)
	     (find-first pred (cdr lis))))))
	    
(define (display-line . items)
  (for-each display items) (newline))
  
(define true? (lambda (x) #t))

(define (get-sentence sentence) (first sentence))
(define (get-premise-set sentence) (second sentence))

(define (rule-name rule) (second rule))
(define (rule-procedure rule) (third rule))
(define (rule-applicability rule) (fourth rule))
(define (rule-id rule) (fifth rule))
(define (rule-group rule) (sixth rule))

(define step-sentence second)
(define step-premise-set third)
(define step-rule fourth)
(define step-premise-sentences fifth)
			    
(define (inference-premise-set inference) (cdr (cdr inference)))
(define (inference-rule-name inference) (rule-name (car inference)))
(define (inference-premise-sentences inference) (car (cdr inference)))

(define (tag x)
  (car x))

;;; splits lst into two based on predicate
;;; car of result is list of positives
;;; cdr of result is list of negatives
(define (split lst pred)
  (let lp ((lst (reverse lst))
  	    (pos (list))
  	    (neg (list)))
    (if (null? lst)
        (cons pos neg)
        (if (pred (car lst))
            (lp (cdr lst) (cons (car lst) pos) neg)
            (lp (cdr lst) pos (cons (car lst) neg))))))

;;; These aren't really utils
;;; But used for testing backbone.scm
(define (demorgans
	 derived
	 psi-and-premise-set)
  (define psi (get-sentence psi-and-premise-set))
  (define psi-premise-set (get-premise-set psi-and-premise-set))
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

(define (demorgans? psi)
  (and (or2? psi)
       (not1? (second psi))
       (not1? (third psi))))

(define (modus-ponens
	 derived
	 phi-and-premise-set
	 psi-and-premise-set)
  (define phi (get-sentence phi-and-premise-set))
  (define phi-premise-set (get-premise-set phi-and-premise-set))
  (define psi (get-sentence psi-and-premise-set))
  (define psi-premise-set (get-premise-set psi-and-premise-set))
  (and
   (implies? phi)
   (equal? psi (second phi))
   (equal? derived (third phi))
   (lset-union equal? phi-premise-set psi-premise-set)))

(define (implies? exp)
  (and (list? exp)
       (= (length exp) 3)
       (eqv? (tag exp) 'implies)))

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
