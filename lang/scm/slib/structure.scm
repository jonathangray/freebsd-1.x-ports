;;; This file was munged by a simple minded sed script since it left
;;; its original authors' hands.  See syncase.doc for the horrid details.

;;; structure.ss
;;; Robert Hieb & Kent Dybvig
;;; 92/06/18

(define-syntax define-structure
   (lambda (x)
      (define construct-name
         (lambda (template-identifier . args)
            (implicit-identifier
               template-identifier
               (string->symbol
                  (apply string-append
                         (map (lambda (x)
                                 (if (string? x)
                                     x
                                     (symbol->string (syntax-object->datum x))))
                              args))))))
      (syntax-case x ()
         ((_ (name id1 ...))
          (syntax (define-structure (name id1 ...) ())))
         ((_ (name id1 ...) ((id2 init) ...))
          (with-syntax
             ((constructor (construct-name (syntax name) "make-" (syntax name)))
              (predicate (construct-name (syntax name) (syntax name) "?"))
              ((access ...)
               (map (lambda (x) (construct-name x (syntax name) "-" x))
                    (syntax (id1 ... id2 ...))))
              ((assign ...)
               (map (lambda (x)
                       (construct-name x "set-" (syntax name) "-" x "!"))
                    (syntax (id1 ... id2 ...))))
              (structure-length
               (+ (length (syntax (id1 ... id2 ...))) 1))
              ((index ...)
               (let f ((i 1) (ids (syntax (id1 ... id2 ...))))
                  (if (null? ids)
                      '()
                      (cons i (f (+ i 1) (cdr ids)))))))
             (syntax (begin
                        (define constructor
                           (lambda (id1 ...)
                              (let* ((id2 init) ...)
                                 (vector 'name id1 ... id2 ...))))
                        (define predicate
                           (lambda (x)
                              (and (vector? x)
                                   (= (vector-length x) structure-length)
                                   (eq? (vector-ref x 0) 'name))))
                        (define access
                           (lambda (x)
                              (vector-ref x index)))
                        ...
                        ; define macro accessors this way:
                        ; (define-syntax access
                        ;       (syntax-case x ()
                        ;          ((_ x)
                        ;           (syntax (vector-ref x index))))))
                        ; ...
                        (define assign
                           (lambda (x update)
                              (vector-set! x index update)))
                        ...)))))))
