;;;; Rationalize

;;; The procedure rationalize is interesting because most programming
;;; languages do not provide anything analogous to it.  For
;;; simplicity, we present an algorithm which computes the correct
;;; result for exact arguments (provided the implementation supports
;;; exact rational numbers of unlimited precision), and produces a
;;; reasonable answer for inexact arguments when inexact arithmetic is
;;; implemented using floating-point.  We thank Alan Bawden for
;;; contributing this algorithm.

(define (rationalize x e)
  (simplest-rational (- x e) (+ x e)))

(define (simplest-rational x y)
  (define (simplest-rational-internal x y)
    ;; assumes 0 < X < Y
    (let ((fx (floor x))
	  (fy (floor y)))
      (cond ((not (< fx x))
	     fx)
	    ((= fx fy)
	     (+ fx
		(/ (simplest-rational-internal
		    (/ (- y fy))
		    (/ (- x fx))))))

	    (else
	     (+ 1 fx)))))
  ;; do some juggling to satisfy preconditions
  ;; of simplest-rational-internal.
  (cond ((< y x)
	 (simplest-rational y x))
	((not (< x y))
	 (if (rational? x) x (slib:error)))
	((positive? x)
	 (simplest-rational-internal x y))
	((negative? y)
	 (- (simplest-rational-internal (- y)
		 (- x))))
	(else
	 (if (and (exact? x) (exact? y))
	     0
	     0.0))))
