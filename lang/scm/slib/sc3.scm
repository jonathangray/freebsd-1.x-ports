;;;; Implementation of rev3 procedures eliminated in subsequent versions.
;;; Copyright (C) 1991 Aubrey Jaffer.

(define (last-pair l)
  (if (pair? (cdr l)) (last-pair (cdr l)) l))

(define t #t)

(define nil #f)

;;;I can't find procedure APPROXIMATE in the Revised^3 Report.
