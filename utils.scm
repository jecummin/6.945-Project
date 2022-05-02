;;; Finds the first element of lis where pred is not #f
;;; Returns the element and the result of pred
(define (find-first pred lis)
  (if (not (list? lis)) (error:not-a list? lis 'find-first))
  (and (not (null? lis))
       (let ((res (pred (car lis))))
	 (if res
	     (cons (car lis) res)
	     (find-first pred (cdr lis))))))

(define true? (lambda (x) #t))

(define (get-sentence sentence) (car sentence))
(define (get-premise-set sentence) (cdr sentence))

(define (tag x)
  (car x))

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
