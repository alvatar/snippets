;;;; logical-combinators.scm
;;;; Kon Lovett, Mar '09

(declare
  (usual-integrations)
  (generic)
  (inline)
  (local)
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#check-closure) )

(module logical-combinators (;export
  andf orf)

(import scheme chicken data-structures srfi-1)

;; Eager 'or' & 'and'

(define (andf . args)
  (let loop ((args args) (prev #t))
    (if (null? args) prev
        (let ((cur (car args)))
          (and cur
               (loop (cdr args) cur) ) ) ) ) )

(define (orf . args)
  (let loop ((args args))
    (cond ((null? args) #f)
          ((car args) => identity)
          (else (loop (cdr args)) ) ) ) )

) ;module logical-combinators
