;;;; prime.scm, prime test and factorization for Scheme
;;; Copyright (C) 1991, 1992, 1993 Aubrey Jaffer.

;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.

;1.  Any copy made of this software must include this copyright notice
;in full.

;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.

;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'random)
(require 'modular)

;;; (modulo p 16) is because we care only about the low order bits.
;;; The odd? tests are inline of (expt -1 ...)

(define (prime:jacobi-symbol p q)
  (cond ((zero? p) 0)
	((= 1 p) 1)
	((odd? p)
	 (if (odd? (quotient (* (- (modulo p 16) 1) (- q 1)) 4))
	     (- (prime:jacobi-symbol (modulo q p) p))
	     (prime:jacobi-symbol (modulo q p) p)))
	(else
	 (let ((qq (modulo q 16)))
	   (if (odd? (quotient (- (* qq qq) 1) 8))
	       (- (prime:jacobi-symbol (quotient p 2) q))
	       (prime:jacobi-symbol (quotient p 2) q))))))

;;;; Solovay-Strassen Prime Test
;;;   if n is prime, then J(a,n) is congruent mod n to a**((n-1)/2)

;;; See:
;;;   Robert Solovay and Volker Strassen,
;;;     "A Fast Monte-Carlo Test for Primality,"
;;;     SIAM Journal on Computing, 1977, pp 84-85.

;;; checks if n is prime.  Returns #f if not prime. #t if (probably) prime.
;;;   probability of a mistake = (expt 2 (- prime:trials))
;;;     choosing prime:trials=30 should be enough
(define prime:trials 30)
;;; prime:product is a product of small primes.
(define prime:product
  (let ((p 210))
    (for-each (lambda (s) (set! p (or (string->number s) p)))
      '("2310" "30030" "510510" "9699690" "223092870"
	       "6469693230" "200560490130"))
    p))

(define (prime:prime? n)
  (set! n (abs n))
  (cond ((<= n 36) (and (memv n '(2 3 5 7 11 13 17 19 23 29 31)) #t))
	((= 1 (gcd n prime:product))
	 (do ((i prime:trials (- i 1))
	      (a (+ 1 (random (- n 1))) (+ 1 (random (- n 1)))))
	     ((not (and (positive? i)
			(= (gcd a n) 1)
			(= (modulo (prime:jacobi-symbol a n) n)
			   (modular:expt n a (quotient (- n 1) 2)))))
	      (if (positive? i) #f #t))))
	(else #f)))

;;;;Lankinen's recursive factoring algorithm:
;From: ld231782@longs.LANCE.ColoState.EDU (L. Detweiler)

;                  |  undefined if n<0,
;                  |  (u,v) if n=0,
;Let f(u,v,b,n) := | [otherwise]
;                  |  f(u+b,v,2b,(n-v)/2) or f(u,v+b,2b,(n-u)/2) if n odd
;                  |  f(u,v,2b,n/2) or f(u+b,v+b,2b,(n-u-v-b)/2) if n even

;Thm: f(1,1,2,(m-1)/2) = (p,q) iff pq=m for odd m.
 
;It may be illuminating to consider the relation of the Lankinen function in
;a `computational hierarchy' of other factoring functions.*  Assumptions are
;made herein on the basis of conventional digital (binary) computers.  Also,
;complexity orders are given for the worst case scenarios (when the number to
;be factored is prime).  However, all algorithms would probably perform to
;the same constant multiple of the given orders for complete composite
;factorizations.
 
;Thm: Eratosthenes' Sieve is very roughtly O(ln(n)/n) in time and
;     O(n*log2(n)) in space.
;Pf: It works with all prime factors less than n (about ln(n)/n by the prime
;    number thm), requiring an array of size proportional to n with log2(n)
;    space for each entry.

;Thm: `Odd factors' is O((sqrt(n)/2)*log2(n)) in time and O(log2(n)) in
;     space.
;Pf: It tests all odd factors less than the square root of n (about
;    sqrt(n)/2), with log2(n) time for each division.  It requires only
;    log2(n) space for the number and divisors.
 
;Thm: Lankinen's algorithm is O(sqrt(n)/2) in time and O((sqrt(n)/2)*log2(n))
;     in space.
;Pf: The algorithm is easily modified to seach only for factors p<q for all
;    pq=m.  Then the recursive call tree forms a geometric progression
;    starting at one, and doubling until reaching sqrt(n)/2, or a length of
;    log2(sqrt(n)/2).  From the formula for a geometric progression, there is
;    a total of about 2^log2(sqrt(n)/2) = sqrt(n)/2 calls.  Assuming that
;    addition, subtraction, comparison, and multiplication/division by two
;    occur in constant time, this implies O(sqrt(n)/2) time and a
;    O((sqrt(n)/2)*log2(n)) requirement of stack space.

(define (prime:f u v b n)
  (if (<= n 0)
      (cond ((negative? n) #f)
	    ((= u 1) #f)
	    ((= v 1) #f)
	    ; Do both of these factors need to be factored?
	    (else (append (or (prime:f 1 1 2 (quotient (- u 1) 2))
			      (list u))
			  (or (prime:f 1 1 2 (quotient (- v 1) 2))
			      (list v)))))
      (if (even? n)
	  (or (prime:f u v (+ b b) (quotient n 2))
	      (prime:f (+ u b) (+ v b) (+ b b) (quotient (- n (+ u v b)) 2)))
	  (or (prime:f (+ u b) v (+ b b) (quotient (- n v) 2))
	      (prime:f u (+ v b) (+ b b) (quotient (- n u) 2))))))

(define (prime:factor m)
  (if
   (negative? m) (cons -1 (prime:factor (- m)))
   (let* ((s (gcd m prime:product))
	  (r (quotient m s)))
     (if (even? s)
	 (append
	  (if (= 1 r) '() (prime:factor r))
	  (cons 2 (let ((s/2 (quotient s 2)))
		    (if (= s/2 1) '()
			(or (prime:f 1 1 2 (quotient (- s/2 1) 2))
			    (list s/2))))))
	 (if (= 1 s) (or (prime:f 1 1 2 (quotient (- m 1) 2)) (list m))
	     (append (if (= 1 r) '()
			 (or (prime:f 1 1 2 (quotient (- r 1) 2)) (list r)))
		     (or (prime:f 1 1 2 (quotient (- s 1) 2)) (list s))))))))

(define jacobi-symbol prime:jacobi-symbol)
(define prime? prime:prime?)
(define factor prime:factor)
