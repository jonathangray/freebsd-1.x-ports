;;;; Pseudo-Random inexact real numbers for scheme.
;;; Copyright (C) 1991, 1993 Aubrey Jaffer.
;;; New sphere and normal functions from: Harald Hanche-Olsen

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

;This file is loaded by random.scm if inexact numbers are supported by
;the implementation.

(define random:float-radix
  (+ 1 (exact->inexact random:MASK)))

;;; This determines how many chunks will be neccessary to completely
;;; fill up an inexact real.
(define (random:size-float l x)
  (if (= 1.0 (+ 1 x))
      l
      (random:size-float (+ l 1) (/ x random:float-radix))))
(define random:chunks/float (random:size-float 1 1.0))

(define (random:uniform-chunk n state)
  (if (= 1 n)
      (/ (exact->inexact (random:chunk state))
	 random:float-radix)
      (/ (+ (random:uniform-chunk (- n 1) state)
	    (exact->inexact (random:chunk state)))
	 random:float-radix)))

;;; Generate an inexact real between 0 and 1.
(define (random:uniform state)
  (random:uniform-chunk random:chunks/float state))

;;; If x and y are independent standard normal variables, then with
;;; x=r*cos(t), y=r*sin(t), we find that t is uniformly distributed
;;; over [0,2*pi] and the cumulative distribution of r is
;;; 1-exp(-r^2/2).  This latter means that u=exp(-r^2/2) is uniformly
;;; distributed on [0,1], so r=sqrt(-2 log u) can be used to generate r.

(define (random:normal-vector! vect . args)
  (let ((state (if (null? args) *random-state* (car args)))
	(sum2 0))
    (let ((do! (lambda (k x)
		 (vector-set! vect k x)
		 (set! sum2 (+ sum2 (* x x))))))
      (do ((n (- (vector-length vect) 1) (- n 2)))
	  ((negative? n) sum2)
	(let ((t (* 6.28318530717958 (random:uniform state)))
	      (r (sqrt (* -2 (log (random:uniform state))))))
	  (do! n (* r (cos t)))
	  (if (positive? n) (do! (- n 1) (* r (sin t)))))))))

(define random:normal
  (let ((vect (make-vector 1)))
    (lambda args 
      (apply random:normal-vector! vect args)
      (vector-ref vect 0))))

;;; For the uniform distibution on the hollow sphere, pick a normal
;;; family and scale.

(define (random:hollow-sphere! vect . args)
  (let ((ms (sqrt (apply random:normal-vector! vect args))))
    (do ((n (- (vector-length vect) 1) (- n 1)))
	((negative? n))
      (vector-set! vect n (/ (vector-ref vect n) ms)))))

;;; For the uniform distribution on the solid sphere, note that in
;;; this distribution the length r of the vector has cumulative
;;; distribution r^n; i.e., u=r^n is uniform [0,1], so r kan be
;;; generated as r=u^(1/n).

(define (random:solid-sphere! vect . args)
  (apply random:hollow-sphere! vect args)
  (let ((r (expt (random:uniform (if (null? args) *random-state* (car args)))
		 (/ (vector-length vect)))))
    (do ((n (- (vector-length vect) 1) (- n 1)))
	((negative? n))
      (vector-set! vect n (* r (vector-ref vect n))))))

(define (random:exp . args)
  (let ((state (if (null? args) *random-state* (car args))))
    (- (log (random:uniform state)))))

(require 'random)
