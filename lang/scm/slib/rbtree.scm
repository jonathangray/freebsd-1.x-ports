;;;; Copyright (C) 1990 Patrick G. Solbavarro.	-*-Scheme-*-
;;;; Red-black trees as in "Introduction to Algorithms," by Cormen, Leiserson,
;;;; and Rivest, chapter 15.

;;;; PGS, 6 Jul 1990
;;; jaffer@ai.mit.edu Ported to SLIB, 1/6/93

(require 'record)
(define rb-tree
  (make-record-type
   "rb-tree"
   '(root left-rotation-field-maintainer right-rotation-field-maintainer
	  insertion-field-maintainer deletion-field-maintainer
	  prior?)))

(define make-rb-tree
  (let ((mrb (record-constructor rb-tree)))
    (lambda (left-rotation-field-maintainer
	     right-rotation-field-maintainer
	     insertion-field-maintainer
	     deletion-field-maintainer
	     prior?)
      (mrb #f left-rotation-field-maintainer right-rotation-field-maintainer
	   insertion-field-maintainer deletion-field-maintainer
	   prior?))))

(define rb-tree-root (record-accessor rb-tree 'root))
(define set-rb-tree-root! (record-modifier rb-tree 'root))
(define rb-tree-left-rotation-field-maintainer
  (record-accessor rb-tree 'left-rotation-field-maintainer))
(define rb-tree-right-rotation-field-maintainer
  (record-accessor rb-tree 'right-rotation-field-maintainer))
(define rb-tree-insertion-field-maintainer
  (record-accessor rb-tree 'insertion-field-maintainer))
(define rb-tree-deletion-field-maintainer
  (record-accessor rb-tree 'deletion-field-maintainer))
(define rb-tree-prior? (record-accessor rb-tree 'prior?))

(define rb-node (make-record-type "rb-node" '(left right parent color data)))
(define make-rb-node
  (let ((mrn (record-constructor rb-node)))
    (lambda (data)
      (mrn #f #f #f 'black data))))

(define rb-node-left (record-accessor rb-node 'left))
(define set-rb-node-left! (record-modifier rb-node 'left))
(define rb-node-right (record-accessor rb-node 'right))
(define set-rb-node-right! (record-modifier rb-node 'right))
(define rb-node-parent (record-accessor rb-node 'parent))
(define set-rb-node-parent! (record-modifier rb-node 'parent))
(define rb-node-color (record-accessor rb-node 'color))
(define set-rb-node-color! (record-modifier rb-node 'color))
(define rb-node-data (record-accessor rb-node 'data))
(define set-rb-node-data! (record-modifier rb-node 'data))

;;;; Rotations
(define (rb-left-rotate tree x)
  (let ((y (rb-node-right x)))
    (let ((beta (rb-node-left y)))
      (set-rb-node-right! x beta)
      ;; make sure x's new child knows who its parent is
      (if beta (set-rb-node-parent! beta x)))
    ;; y is now x's parent's child
    (let ((subtree-parent (rb-node-parent x)))
      (set-rb-node-parent! y subtree-parent)
      ;; if x was tree root, y is now
      (if (not subtree-parent)
	  (set-rb-tree-root! tree y)
	  ;; otherwise if x wasn't tree root, have to figure out which child
	  ;; it was, so we can update parent's corresponding child field.
	  (if (eq? x (rb-node-left subtree-parent))
	      (set-rb-node-left! subtree-parent y)
	      (set-rb-node-right! subtree-parent y))))
    ;; now x is y's left child
    (set-rb-node-left! y x)
    ;; and y is x's parent
    (set-rb-node-parent! x y)
    ;; invoke augmented field maintenance routine if there is one
    (let ((augmented-field-maintenance-routine
	   (rb-tree-left-rotation-field-maintainer tree)))
      (if augmented-field-maintenance-routine
	  (augmented-field-maintenance-routine x y)))))

(define (rb-right-rotate tree y)
  (let ((x (rb-node-left y)))
    (let ((beta (rb-node-right x)))
      (set-rb-node-left! y beta)
      ;; make sure y's new child knows who its parent is
      (if beta (set-rb-node-parent! beta y)))
    ;; x is now y's parent's child
    (let ((subtree-parent (rb-node-parent y)))
      (set-rb-node-parent! x subtree-parent)
      ;; if y was tree root, x is now
      (if (not subtree-parent)
	  (set-rb-tree-root! tree x)
	  ;; otherwise if y wasn't tree root, have to figure out which child
	  ;; it was, so we can update parent's corresponding child field.
	  (if (eq? y (rb-node-right subtree-parent))
	      (set-rb-node-right! subtree-parent x)
	      (set-rb-node-left! subtree-parent x))))
    ;; now y is x's right child
    (set-rb-node-right! x y)
    ;; and x is y's parent
    (set-rb-node-parent! y x)
    ;; invoke augmented field maintenance routine if there is one
    (let ((augmented-field-maintenance-routine
	   (rb-tree-right-rotation-field-maintainer tree)))
      (if augmented-field-maintenance-routine
	  (augmented-field-maintenance-routine x y)))))


;;;; Insertion.

(define (rb-insert! tree x)
  ;; normal binary tree insertion
  (define (rb-binary-tree-insert tree z)
    (let ((prior? (rb-tree-prior? tree))
	  (y #f)
	  (z-data (rb-node-data z)))
      (do ((x (rb-tree-root tree)))
	  ((not x))
	(set! y x)
	(if (prior? z-data (rb-node-data x))
	    ;; descend left
	    (set! x (rb-node-left x))
	    ;; descend right
	    (set! x (rb-node-right x))))
      ;; link z in under y
      (set-rb-node-parent! z y)
      ;; if y was null, z is now the root of the tree
      (if (not y)
	  (set-rb-tree-root! tree z)
	  ;; but otherwise have to make z appropriate child of y
	  (if (prior? z-data (rb-node-data y))
	      (set-rb-node-left! y z)
	      (set-rb-node-right! y z)))))
  ;; start by doing normal binary tree insertion
  (rb-binary-tree-insert tree x)
  (let ((augmented-field-maintenance-routine
	 (rb-tree-insertion-field-maintainer tree)))
    (if augmented-field-maintenance-routine
	(augmented-field-maintenance-routine x)))
  (set-rb-node-color! x 'red)
  (do ((y 'uninitialized))
      ((or (eq? x (rb-tree-root tree))
	   (not (eq? (rb-node-color (rb-node-parent x)) 'red))))
    ;; if x's parent is a left child of its grandparent
    (if (eq? (rb-node-parent x)
	     (rb-node-left (rb-node-parent (rb-node-parent x))))
	(begin
	  ;; get other child of x's grandparent
	  (set! y (rb-node-right (rb-node-parent (rb-node-parent x))))
	  ;; if uncle was red
	  (if (and y (eq? (rb-node-color y) 'red))
	      ;; making grandparent red, maintain lower invariants
	      (begin
		(set-rb-node-color! (rb-node-parent x) 'black)
		(set-rb-node-color! y 'black)
		(set-rb-node-color! (rb-node-parent (rb-node-parent x)) 'red)
		(set! x (rb-node-parent (rb-node-parent x))))
	      ;; if uncle was black,
	      (begin
		;; if x is a right child,
		(cond ((eq? x (rb-node-right (rb-node-parent x)))
		       ;; left-rotate about parent
		       (set! x (rb-node-parent x))
		       (rb-left-rotate tree x)))
		(set-rb-node-color! (rb-node-parent x) 'black)
		(set-rb-node-color! (rb-node-parent (rb-node-parent x)) 'red)
		(rb-right-rotate tree (rb-node-parent (rb-node-parent x))))))
	;; if x's parent is a right child of its grandparent
	(begin
	  ;; get other child of x's grandparent
	  (set! y (rb-node-left (rb-node-parent (rb-node-parent x))))
	  ;; if uncle was red
	  (if (and y (eq? (rb-node-color y) 'red))
	      ;; making grandparent red, maintain lower invariants
	      (begin
		(set-rb-node-color! (rb-node-parent x) 'black)
		(set-rb-node-color! y 'black)
		(set-rb-node-color! (rb-node-parent (rb-node-parent x)) 'red)
		(set! x (rb-node-parent (rb-node-parent x))))
	      (begin
		;; if x is a left child,
		(cond ((eq? x (rb-node-left (rb-node-parent x)))
		       ;; right-rotate about parent
		       (set! x (rb-node-parent x))
		       (rb-right-rotate tree x)))
		(set-rb-node-color! (rb-node-parent x) 'black)
		(set-rb-node-color! (rb-node-parent (rb-node-parent x)) 'red)
		(rb-left-rotate tree (rb-node-parent (rb-node-parent x))))))))
  (set-rb-node-color! (rb-tree-root tree) 'black))

;;;; Queries
(define (rb-node-minimum node)
  (let ((node-left (rb-node-left node)))
    (if node-left
	(rb-node-minimum node-left)
	node)))

(define (rb-node-maximum node)
  (let ((node-right (rb-node-right node)))
    (if node-right
	(rb-node-maximum node-right)
	node)))


(define (rb-tree-minimum tree)
  (rb-node-minimum (rb-tree-root tree)))

(define (rb-tree-maximum tree)
  (rb-node-maximum (rb-tree-root tree)))

(define (rb-node-successor x)
  (let ((node-right (rb-node-right x)))
    (if node-right (rb-node-minimum node-right)
	(do ((y (rb-node-parent x)))
	    ((or (not y) (not (eq? x (rb-node-right y))))
	     y)
	  (set! x y)
	  (set! y (rb-node-parent y))))))

(define (rb-node-predecessor x)
  (if (rb-node-left x) (rb-node-minimum (rb-node-left x))
      (do ((y (rb-node-parent x)))
	  ((or (not y) (not (eq? x (rb-node-left y))))
	   y)
	(set! x y)
	(set! y (rb-node-parent y)))))


;;;; Deletion.  We do not entirely follow Cormen, Leiserson and Rivest's lead
;;;; here, because their use of sentinels is in rather obscenely poor taste.
;;;; Instead, we pass X's parent to RB-DELETE-FIXUP and check explicitly for
;;;; the null case.

;;; The node that is actually deleted may not be the one passed in, so if a
;;; resource is being maintained, what should be put back on the freelist is
;;; the node returned by this procedure.
(define (rb-delete! tree z)
  ;; first part is usual binary tree deletion
  (let* ((y 'uninitialized)
	 (x 'uninitialized))
    (if (or (not (rb-node-left z)) (not (rb-node-right z)))
	;; if node to delete has only one child or none, can just splice it
	;; out
	(set! y z)
	;; if node to delete has two children, find its successor (which has
	;; only one child) and splice successor in in place of deleted node
	(set! y (rb-node-successor z)))
    ;; know at this point that y has at most one child; get it in x
    (if (rb-node-left y)
	(set! x (rb-node-left y))
	(set! x (rb-node-right y)))
    ;; we'll want this later
    (let ((y-parent (rb-node-parent y)))
      ;; this child takes y's place.
      (if x (set-rb-node-parent! x (rb-node-parent y)))
      ;; if y was the root, have to update the tree
      (if (not y-parent)
	  (set-rb-tree-root! tree x)
	  ;; if y wasn't root, have to tell y's parent about new child x
	  (if (eq? y (rb-node-left y-parent))
	      (set-rb-node-left! y-parent x)
	      (set-rb-node-right! y-parent x)))
      (let ((deletion-field-maintenance-routine
	     (rb-tree-deletion-field-maintainer tree))
	    (insertion-field-maintenance-routine
	     (rb-tree-insertion-field-maintainer tree)))
	;; if we have a deletion field maintainer, use it to make tree
	;; consistent with y's removal.
	(if deletion-field-maintenance-routine
	    (deletion-field-maintenance-routine y))
	;; if y was actually z's successor, we aren't really deleting y but z,
	;; and inserting y in z's place.  So update z's data field to y's.
	(cond ((not (eq? y z))
	       (cond (deletion-field-maintenance-routine
		      (deletion-field-maintenance-routine z) ;deleting z
		      (insertion-field-maintenance-routine y))) ;inserting y
	       (set-rb-node-data! z (rb-node-data y)))))
      ;; clean up tree if we've unbalanced it
      (if (eq? (rb-node-color y) 'black)
	  (rb-delete-fixup tree x y-parent)))
    y))

;;; This routine makes the red-black tree a legal red-black tree again.  At
;;; entry, X is a node that is "doubly black."  X-PARENT is passed in case X
;;; is actually null.
(define (rb-delete-fixup tree x x-parent)
  (do ((w 'uninitialized))
      ;; done when x is root or no longer black
      ((or (eq? x (rb-tree-root tree))
	   (not (or (not x)		;x is black if x is null
		    (eq? (rb-node-color x) 'black)))))
    (if (eq? x (rb-node-left x-parent))
	;; note that w cannot be NIL, by red-black tree invariants, because
	;; x is doubly black, and otherwise the black-counts on the branches
	;; would be different.
	(begin (set! w (rb-node-right x-parent))
	       ;; if w is red make it black and rotate
	       (cond ((eq? (rb-node-color w) 'red)
		      (set-rb-node-color! w 'black)
		      (set-rb-node-color! x-parent 'red)
		      (rb-left-rotate tree x-parent)
		      ;; this new w can't be NIL either, by same argument
		      (set! w (rb-node-right x-parent))))
	       ;; if both of w's children are black
	       (if (and (or (not (rb-node-left w))
			    (eq? (rb-node-color (rb-node-left w)) 'black))
			(or (not (rb-node-right w))
			    (eq? (rb-node-color (rb-node-right w)) 'black)))
		   (begin (set-rb-node-color! w 'red) ;make w red
			  (set! x x-parent) ;move up tree
			  (set! x-parent (rb-node-parent x)))
		   (begin
		     (cond ((or (not (rb-node-right w))
				(eq? (rb-node-color (rb-node-right w)) 'black))
			    ;; know left isn't NIL, or IF would have succeeded
			    (set-rb-node-color! (rb-node-left w) 'black)
			    (set-rb-node-color! w 'red)
			    (rb-right-rotate tree w)
			    (set! w (rb-node-right x-parent))))
		     (set-rb-node-color! w (rb-node-color x-parent))
		     (set-rb-node-color! x-parent 'black)
		     (if (rb-node-right w)
			 (set-rb-node-color! (rb-node-right w) 'black))
		     (rb-left-rotate tree x-parent)
		     (set! x (rb-tree-root tree)))))
	;; W can't be NIL here either, as above
	(begin (set! w (rb-node-left x-parent))
	       ;; if w is red make it black and rotate
	       (cond ((eq? (rb-node-color w) 'red)
		      (set-rb-node-color! w 'black)
		      (set-rb-node-color! x-parent 'red)
		      (rb-right-rotate tree x-parent)
		      ;; **are we still okay in referencing x-parent here?
		      (set! w (rb-node-left x-parent))))
	       ;; if both of w's children are black
	       (if (and (or (not (rb-node-right w))
			    (eq? (rb-node-color (rb-node-right w)) 'black))
			(or (not (rb-node-left w))
			    (eq? (rb-node-color (rb-node-left w)) 'black)))
		   (begin (set-rb-node-color! w 'red) ;make w red
			  (set! x x-parent) ;move up tree
			  (set! x-parent (rb-node-parent x)))
		   (begin
		     (cond ((or (not (rb-node-left w))
				(eq? (rb-node-color (rb-node-left w)) 'black))
			    ;; know right isn't NIL, or IF would have succeeded
			    (set-rb-node-color! (rb-node-right w) 'black)
			    (set-rb-node-color! w 'red)
			    (rb-left-rotate tree w)
			    (set! w (rb-node-left x-parent))))
		     (set-rb-node-color! w (rb-node-color x-parent))
		     (set-rb-node-color! x-parent 'black)
		     (if (rb-node-left w)
			 (set-rb-node-color! (rb-node-left w) 'black))
		     (rb-right-rotate tree x-parent)
		     (set! x (rb-tree-root tree)))))))
  (if x (set-rb-node-color! x 'black)))
