;;;; Pseudo-Random number generator for scheme.
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

(define random:tap 24)
(define random:size 55)

(define (random:size-int l)
  (let ((trial (string->number (make-string l #\f) 16)))
  (if (and (exact? trial) (>= most-positive-fixnum trial))
      l
      (random:size-int (- l 1)))))
(define random:chunk-size (* 4 (random:size-int 8)))

(define random:MASK
  (string->number (make-string (quotient random:chunk-size 4) #\f) 16))

(define *random-state*
  '#(
 "d909ef3e" "fd330ab3" "e33f7843" "76783fbd" "f3675fb3"
 "b54ef879" "0be45590" "a6794679" "0bcd56d3" "fabcdef8"
 "9cbd3efd" "3fd3efcd" "e064ef27" "dddecc08" "34444292"
 "85444454" "4c519210" "c0366273" "54734567" "70abcddc"
 "1bbdac53" "616c5a86" "a982efa9" "105996a0" "5f0cccba"
 "1ea055e1" "fe2acd8d" "1891c1d4" "e6690270" "6912bccc"
 "2678e141" "61222224" "907abcbb" "4ad6829b" "9cdd1404"
 "57798841" "5b892496" "871c9cd1" "d1e67bda" "8b0a3233"
 "578ef23f" "28274ef6" "823ef5ef" "845678c5" "e67890a5"
 "5890abcb" "851fa9ab" "13efa13a" "b12278d6" "daf805ab"
 "a0befc36" "0068a7b5" "e024fd90" "a7b690e2" "27f3571a"
 0))

(let ((random-strings *random-state*))
  (set! *random-state* (make-vector (+ random:size 1) 0))
  (let ((nibbles (quotient random:chunk-size 4)))
    (do ((i 0 (+ i 1)))
	((= i random:size))
      (vector-set!
       *random-state* i
       (string->number (substring (vector-ref random-strings i)
				  0 nibbles)
		       16)))))

;;; random:chunk returns an integer in the range of
;;; 0 to (- (expt 2 random:chunk-size) 1)
(define (random:chunk v)
  (let* ((p (vector-ref v random:size))
	 (ans (logical:logxor
	       (vector-ref v (modulo (- p random:tap) random:size))
	       (vector-ref v p))))
    (vector-set! v p ans)
    (vector-set! v random:size (modulo (- p 1) random:size))
    ans))

(define (random:random modu . args)
  (let ((state (if (null? args) *random-state* (car args))))
    (if (exact? modu)
	(let ((ilen (do ((n 0 (+ 1 n))
			 (s random:MASK
			    (+ random:MASK (* (+ 1 random:MASK) s))))
			((>= s modu) n)))
	      (slop (modulo (+ random:MASK (- 1 modu)) modu)))
	  (let loop ((n ilen)
		     (r (random:chunk state)))
	    (cond ((not (zero? n))
		   (loop (+ -1 n)
			 (+ (* r (+ 1 random:MASK)) (random:chunk state))))
		  ((>= r slop) (modulo r modu))
		  (else (loop ilen (random:chunk state))))))
	(* (random:uniform state) modu))))
;;;random:uniform is in randinex.scm.  It is needed only if inexact is
;;;supported.

(define (random:make-random-state . args)
  (let ((state (if (null? args) *random-state* (car args))))
    (list->vector (vector->list state))))

(define random random:random)
(define make-random-state random:make-random-state)

(provide 'random)			;to prevent loops
(if (provided? 'inexact) (require 'random-inexact))
