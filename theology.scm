;; Definition of a simple set of theological inference rules
;; for theology defined on a small subset of English.

(define (begins-with? sentence prefix)
  (let lp ((sent sentence) (pre prefix))
    (if (null? pre)
	#t
	(if (equal? (car sent) (car pre))
	    (lp (cdr sent) (cdr pre))
	    #f))))

(define (contains? sentence x)
  (let lp ((sent sentence))
    (if (null? sent)
	#f
	(if (equal? (car sent) x)
	    #t
	    (lp (cdr sent))))))

(define (get-prefix sentence x)
  (let lp ((sent sentence) (new '()))
    (if (null? sent)
	new
	(if (equal? (car sent) x)
	    new
	    (lp (cdr sent) (append new (list (car sent))))))))

(define (get-suffix sentence x)
  (let lp ((sent sentence) (new '()) (found #f))
    (if (null? sent)
	new
	(if found
	    (lp (cdr sent) (append new (list (car sent))) #t)
	    (if (equal? (car sent) x)
		(lp (cdr sent) new #t)
		(lp (cdr sent) new #f))))))


(define (the-word? x)
  (or (begins-with? x '(God said))
      (begins-with? x '(Scripture teaches))))

(define (and? x)
  (contains? x 'and))

(define (and-first x)
  (get-prefix x 'and))

(define (and-second x)
  (get-suffix x 'and))

(define (or? x)
  (contains? x 'or))

(define (or-first x)
  (get-prefix x 'or))

(define (or-second x)
  (get-suffix x 'or))

(define (therefore? x)
  (contains? x 'therefore))

(define (therefore-first x)
  (get-prefix x 'therefore))

(define (therefore-second x)
  (get-suffix x 'therefore))

(define (sentence? x)
  #t)

(define (sentence-eqv? x y)
  (equal? x y))



;; the-word-is-true-applicability target
;; (the-word? target)
(define (the-word-is-true-rule-1 target)
  '())


(define-rule!
  'the-word-is-true
  the-word-is-true-rule-1
  (list the-word?)
  'theology)

(define (the-word-contents x)
  (cddr x))

(define (the-word-is-true-rule-2 target word-and-premises)
  (let ((word (car word-and-premises))
	(word-premise-set (cadr word-and-premises)))
    (if (sentence-eqv? target (the-word-contents word))
	'()
	#f)))


(define-rule!
  'the-word-is-true
  the-word-is-true-rule-2
  (list sentence? the-word?)
  'theology)

;; theologian-and-applicability target conj
;; (sentence? target)
;; (and? conj)
(define (theologian-and-rule target conj-and-premises)
  (let ((conj (car conj-and-premises))
	(premise-set (cadr conj-and-premises)))
    (cond
     ((sentence-eqv? target (and-first conj)) premise-set)
     ((sentence-eqv? target (and-second conj)) premise-set)
     (else #f))))

(define-rule!
  'theologian-and
  theologian-and-rule
  (list sentence? and?)
  'theology)

  
;; theologian-or-applicability target disj sent
;; (sentence? target)
;; (or? disj)
;; (sentence? sent)
(define (theologian-or-rule target disj-and-premises
			    sent-and-premises)
  (let ((disj (car disj-and-premises))
	(disj-premise-set (cadr disj-and-premises))
	(sent (car sent-and-premises))
	(sent-premise-set (cadr sent-and-premises)))
    (if (or (and (sentence-eqv? sent (or-first disj))
		 (sentence-eqv? target (or-second disj)))
	    (and (sentence-eqv? sent (or-second disj))
		 (sentence-eqv? target (or-first disj))))
	(append disj-premise-set sent-premise-set)
	#f)))


(define-rule!
  'theologian-or
  theologian-or-rule
  (list sentence? or? sentence?)
  'theology)



;; theologian-therefore-applicability derived phi chi
;; (therefore? derived)
;; (sentence? phi)
;; (sentence? psi)
(define (theologian-therefore-rule derived phi-and-premises psi-and-premises)
  (let ((phi (car phi-and-premises))
	(phi-premise-set (cadr phi-and-premises))
	(psi (car psi-and-premises))
	(psi-premise-set (cadr psi-and-premises)))
  (if (and (sentence-eqv? (therefore-first derived) phi)
	   (sentence-eqv? (therefore-second derived) psi))
      (append phi-premise-set  psi-premise-set) ;; Not the same as CP !!
      #f)))

(define-rule!
  'theologian-therefore
  theologian-therefore-rule
  (list therefore? sentence? sentence?)
  'theology)
