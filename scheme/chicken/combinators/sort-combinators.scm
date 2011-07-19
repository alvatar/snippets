;;;; sort-combinators.scm
;;;; Kon Lovett, Mar '09

(declare
  (usual-integrations)
  (generic)
  (inline)
  (local)
  (no-procedure-checks)
  (bound-to-procedure
    ##sys#check-closure) )

(module sort-combinators (;export
  group/key
  make-equal/key make-less-than/key)

(import scheme chicken srfi-1)

(require-library srfi-1)

;; Group a list of elements by some key attribute.
;;
;; The list must be in sorted order with respect to the key.
;;
;; examples:
;; (group/key identity '(1 2 3 3 4 4 4)) --> ((1) (2) (3 3) (4 4 4))
;; (group/key car '((a 1) (a 2) (b 1))) --> '(((a 1) (a 2)) ((b 1)))

(define (group/key keyproc ls #!optional (equality equal?))
  (##sys#check-closure keyproc 'group/key)
  (##sys#check-closure equality 'group/key)
  (let loop ((ls ls) (acc '()))
    (if (null? ls) acc #;(reverse! acc)
        (let ((key (keyproc (car ls))))
          (receive (grouped rest) (span (lambda (item) (equality key (keyproc item))) ls)
            (loop rest (cons grouped acc)) ) ) ) ) )

;; Define a less-than function for a sort of a structured sequence.
;;
;; E.g. to sort a list of lists by their first items, using
;; string-case-insensitive comparison:
;; (sort ls (make-less-than/key first string-ci<?))

(define (make-less-than/key keyproc #!optional (less-than <))
  (##sys#check-closure keyproc 'make-less-than/key)
  (##sys#check-closure less-than 'make-less-than/key)
  (lambda (a b) (less-than (keyproc a) (keyproc b)) ) )

;; Define a equal function for a sort of a structured sequence.
;;
;; E.g. to sort a list of lists by their first items, using
;; string-case-insensitive comparison:
;; (make-hash-table (uni first string-ci-hash) (make-equal/key first string-ci=?))

(define (make-equal/key keyproc #!optional (equal =))
  (##sys#check-closure keyproc 'make-equal/key)
  (##sys#check-closure equal 'make-equal/key)
  (lambda (a b) (equal (keyproc a) (keyproc b)) ) )

) ;module sort-combinators
