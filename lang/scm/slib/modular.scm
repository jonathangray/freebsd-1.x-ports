;;;; modular.scm, modular fixnum arithmetic for Scheme
;;; Copyright (C) 1991, 1993 Aubrey Jaffer.

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

(require 'logical)

(define (modular:extended-euclid x y)
  (define q 0)
  (do ((r0 x r1) (r1 y (remainder r0 r1))
       (u0 1 u1) (u1 0 (- u0 (* q u1)))
       (v0 0 v1) (v1 1 (- v0 (* q v1))))
      ;; (assert (= r0 (+ (* u0 x) (* v0 y))))
      ;; (assert (= r1 (+ (* u1 x) (* v1 y))))
      ((zero? r1) (list r0 u0 v0))
    (set! q (quotient r0 r1))))

(define (modular:invert m a)
  (let ((d (modular:extended-euclid a m)))
    (if (= 1 (car d))
	(modulo (cadr d) m)
	(slib:error "modular:invert can't invert" m a))))

(define (modular:negate m a) (if (zero? a) 0 (- m a)))

;;; Being careful about overflow here
(define (modular:+ m a b) (modulo (+ (- a m) b) m))

(define (modular:- m a b) (modulo (- a b) m))

;;; See: L'Ecuyer, P. and Cote, S. "Implementing a Random Number Package
;;; with Splitting Facilities." ACM Transactions on Mathematical
;;; Software, 17:98-111 (1991)

;;; modular:r = 2**((nb-2)/2) where nb = number of bits in a word.
(define modular:r
  (ash 1 (quotient (integer-length most-positive-fixnum) 2)))
(define modular:*
  (if (provided? 'bignum) (lambda (m a b) (modulo (* a b) m))
      (lambda (m a b)
	(let ((a0 a)
	      (p 0))
	  (cond ((< a modular:r))
		((< b modular:r) (set! a b) (set! b a0) (set! a0 a))
		(else
		 (set! a0 (modulo a modular:r))
		 (let ((a1 (quotient a modular:r))
		       (qh (quotient m modular:r))
		       (rh (modulo m modular:r)))
		   (cond ((>= a1 modular:r)
			  (set! a1 (- a1 modular:r))
			  (set! p (modulo (- (* modular:r (modulo b qh))
					     (* (quotient b qh) rh)) m))))
		   (cond ((not (zero? a1))
			  (let ((q (quotient m a1)))
			    (set! p (- p (* (quotient b q) (modulo m a1))))
			    (set! p (modulo (+ (if (positive? p) (- p m) p)
					       (* a1 (modulo b q))) m)))))
		   (set! p (modulo (- (* modular:r (modulo p qh))
				      (* (quotient p qh) rh)) m)))))
	  (if (zero? a0)
	      p
	      (let ((q (quotient m a0)))
		(set! p (- p (* (quotient b q) (modulo m a0))))
		(modulo (+ (if (positive? p) (- p m) p)
			   (* a0 (modulo b q))) m)))))))

(define (modular:expt m a b)
  (cond ((= a 1) 1)
	((= a (- m 1)) (if (odd? b) a 1))
	((zero? a) 0)
	(else
	 (logical:ipow-by-squaring a b 1
				   (lambda (c d) (modular:* m c d))))))

(define extended-euclid modular:extended-euclid)
