;;;; Implementation of COMMON LISP tree functions for Scheme
;;; from d.love@dl.ac.uk (some of this may be adapted from T sources):

(define (TREE:COPY-TREE tree)	; from Dybvig (called tree-copy there)
  (if (not (pair? tree))
      tree
      (cons (tree:copy-tree (car tree))
	    (tree:copy-tree (cdr tree)))))

(define (TREE:SUBST new old tree)
    (cond ((equal? old tree) new)
          ((pair? tree)
           (if (equal? (car tree) old)
	       (cons new (tree:subst new old (cdr tree)))
	       (cons (tree:subst new old (car tree))
		     (tree:subst new old (cdr tree)))))
          (else tree)))

;; the next 2 aren't in CL (names from Dybvig)
(define (TREE:SUBSTQ new old tree)
    (cond ((eq? old tree) new)
          ((pair? tree)
           (if (eq? (car tree) old)
	       (cons new (tree:substq new old (cdr tree)))
	       (cons (tree:substq new old (car tree))
		     (tree:substq new old (cdr tree)))))
          (else tree)))

(define (TREE:SUBSTV new old tree)
    (cond ((eqv? old tree) new)
          ((pair? tree)
           (if (eqv? (car tree) old)
	       (cons new (tree:substv new old (cdr tree)))
	       (cons (tree:substv new old (car tree))
		     (tree:substv new old (cdr tree)))))
          (else tree)))

(define copy-tree tree:copy-tree)
(define subst tree:subst)
(define substq tree:substq)
(define substv tree:substv)
