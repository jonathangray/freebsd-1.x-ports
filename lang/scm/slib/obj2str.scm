;;; "obj2str.scm"
(require 'string-port)

(define (object->string obj)
  (cond ((symbol? obj) (symbol->string obj))
	((number? obj) (number->string obj))
	(else
	 (call-with-output-string
	  (lambda (port) (write obj port))))))

; File: "obj2str.scm"   (c) 1991, Marc Feeley

;(require 'generic-write)

; (object->string obj) returns the textual representation of 'obj' as a
; string.
;
; Note: (write obj) = (display (object->string obj))

;(define (object->string obj)
;  (let ((result '()))
;    (generic-write obj #f #f (lambda (str) (set! result (cons str result)) #t))
;    (reverse-string-append result)))

; (object->limited-string obj limit) returns a string containing the first
; 'limit' characters of the textual representation of 'obj'.

;(define (object->limited-string obj limit)
;  (let ((result '()) (left limit))
;    (generic-write obj #f #f
;      (lambda (str)
;        (let ((len (string-length str)))
;          (if (> len left)
;            (begin
;              (set! result (cons (substring str 0 left) result))
;              (set! left 0)
;              #f)
;            (begin
;              (set! result (cons str result))
;              (set! left (- left len))
;              #t)))))
;    (reverse-string-append result)))
