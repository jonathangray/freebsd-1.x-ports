;;;; Copyright (C) 1990 Patrick G. Solbavarro.	-*-Scheme-*-
;;; Test rbtree.scm

(require 'red-black-tree)
(require 'format)

;;;; For debugging.  LEFT-THUNK called with no args before descending left;
;;;; RIGHT-THUNK with no args after ascending right.
(define (rb-tree-in-order-walk tree left-thunk node-thunk right-thunk)
  (define (rb-node-in-order-walk node left-thunk node-thunk right-thunk)
    (if (not node)
	#f
	(begin (left-thunk)
	       (rb-node-in-order-walk
		(rb-node-left node) left-thunk node-thunk right-thunk)
	       (node-thunk node)
	       (rb-node-in-order-walk
		(rb-node-right node) left-thunk node-thunk right-thunk)
	       (right-thunk))))
  (rb-node-in-order-walk (rb-tree-root tree) left-thunk node-thunk right-thunk))

(define (show-rb-tree tree)
  (rb-tree-in-order-walk
   tree
   (lambda () (format #t "["))
   (lambda (node) (format #t "<~s ~s>" (rb-node-data node) (rb-node-color node)))
   (lambda () (format #t "]")))
  (format #t "~%"))

;;; if Scheme had RANDOM, I'd use that
(define (build-test-tree)
  (let ((new-tree (make-rb-tree #f #f #f #f <)))
    (let ((contents '(61 65 66 13 50 43 77 93 91 8 59 76 94 38 20 64 5 37 51
			 23)))
      (do ((contents-tail contents (cdr contents-tail)))
	  ((null? contents-tail))
	(rb-insert! new-tree (make-rb-node (car contents-tail)))))
    new-tree))

(define (flush-test-tree tree)
  (define (rb-tree-nth tree n)
    (define (rb-node-nth node n)
      (if (= n 0)
	  node
	  (rb-node-nth (rb-node-successor node) (- n 1))))
    (rb-node-nth (rb-tree-minimum tree) n))
  ;; removal indices crafted so that never remove elt beyond remaining elts
  (let ((removal-indices '(12 1 7 3 0 7 10 11 3 8 6 4 5 2 4 1 0 0 1 0)))
    ;; show tree
    (show-rb-tree tree)
    ;; now remove items
    (do ((removal-indices-tail removal-indices (cdr removal-indices-tail)))
	((null? removal-indices-tail))
      (let ((node-to-delete (rb-tree-nth tree (car removal-indices-tail))))
	(format #t "Deleting node with datum ~S~%" (rb-node-data node-to-delete))
	(rb-delete! tree node-to-delete))
      (show-rb-tree tree))))

(flush-test-tree (build-test-tree))
