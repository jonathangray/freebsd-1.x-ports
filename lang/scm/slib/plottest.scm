(require 'charplot)
(require 'random)

(define strophoid
  (let ((l '()))
    (do ((x -1.0 (+ x 0.05)))
	((> x 4.0))
      (let* ((a (/ (- 2 x) (+ 2 x))))
	(if (>= a 0.0)
	    (let* ((y (* x (sqrt a))))
	      (set! l (cons (cons x y) l))
	      (set! l (cons (cons x (- y)) l))))))
    l))

(plot! strophoid "x" "y") (newline)

(define unif
  (let* ((l 6)
	 (v (make-vector l)))
    (do ((i (- l 1) (- i 1)))
	((negative? i))
      (vector-set! v i (cons i 0)))
    (do ((i 24 (- i 1))
	 (r (random l) (random l)))
	((zero? i) (vector->list v))
      (set-cdr! (vector-ref v r) (+ 1 (cdr (vector-ref v r)))))))

(plot! unif "n" "occur")
