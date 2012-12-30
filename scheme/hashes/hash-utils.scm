;;;; hash-utils.scm
;;;; Kon Lovett, Jan '06
;;;; Kon Lovett, Aug '10

;; Issues
;;
;; - unsigned-integer32-ref & -set! assume all object data aligned
;; on 32-bit boundary!

(module hash-utils

  (;export
    current-hash-seed
    make-range-restriction
    make-fixnum-range-restriction
    make-seeded-hash
    make-mask-hash
    make-range-hash
    make-bounded-hash
    make-fixnum-bounded-hash
    make-real-hash
    make-hash-procedure
    make-hash-message-digest-primitive
    ; reexport
    unsigned-integer32-set!
    unsigned-integer32-ref)

  (import scheme chicken foreign)

  (use message-digest miscmacros moremacros type-checks type-errors)
  (use hashes-support)

#>
#include "hashes.h"
#undef bitsizeof
<#

(include "hashes-macros")

;;;

(define-inline (check-unsigned-integer32 loc obj)
	(unless (and (integer? obj) (<= 0 obj maximum-unsigned-integer32))
		(error-argument-type loc obj "unsigned-integer32") )
  obj )

;;

(define-inline (force-exact num)
  (if (flonum? num) (##core#inline "C_quickflonumtruncate" num)
    num) )

(define-inline (number->exact n)
	(if (exact? n) n
    (let ((i (if (integer? n) n (round n))))
      (inexact->exact
        (cond ((< i most-negative-fixnum) (modulo i most-negative-fixnum))
              ((< most-positive-fixnum i) (modulo i most-positive-fixnum))
              (else                       i))) ) ) )

(define-inline (number->fixnum n)
  (force-exact (number->exact n)) )

;; Takes 1 argument:
;; hash-primitive-procedure
;;
;; Returns 1 value:
;; hash-update-procedure
;; ((c-pointer "ctx") scheme-object unsigned-integer32 -> void)

(define-inline (%make-hash-update-procedure prim-proc)
  (lambda (ctx data length)
    (ctx-hash-set! ctx (prim-proc data length (ctx-hash-ref ctx))) ) )

;;;

;; Common hash seed

(define-parameter current-hash-seed DEFAULT-HASH-SEED
	(lambda (v)
	  (hash-seed
      (cond
        ((not v)        0)
        ((integer? v)   (if (negative? v) (- v) v))
        (else
          (warning-argument-type 'current-hash-seed v "hash-seed")
          (hash-seed) ) ) )
    (hash-seed)))

;;; Range restrictions

(define (make-range-restriction upper . args)
	(let-optionals args ((lower 0))
		(check-number 'make-range-restriction lower)
		(check-number 'make-range-restriction upper)
		(unless (<= lower upper) (swap-set! upper lower))
		(if (zero? lower) (lambda (num) (modulo num upper))
      (let ((diff (- upper lower -1)))
        (lambda (num) (+ lower (modulo num diff))) ) ) ) )

(define (make-fixnum-range-restriction upper . args)
	(let-optionals args ((lower 0))
		(check-fixnum 'make-fixnum-range-restriction lower)
		(check-fixnum 'make-fixnum-range-restriction upper)
		(unless (<= lower upper) (swap-set! upper lower))
		(if (fx= 0 lower) (lambda (num) (fxmod (number->fixnum num) upper))
      (let ((diff (fx+ (fx- upper lower) 1)))
        (lambda (num) (fx+ lower (fxmod (number->fixnum num) diff))) ) ) ) )

;;; SRFI-69 hash function signatures

(define (make-fixnum-bounded-hash hash-proc . args)
  (let-optionals args ((byte-length ##sys#size) (initr #f))
    (check-procedure 'make-fixnum-bounded-hash hash-proc)
    (check-procedure 'make-fixnum-bounded-hash byte-length)
    (when initr (check-procedure 'make-fixnum-bounded-hash initr))
    (lambda (obj . args)
      (let-optionals args ((bound most-positive-fixnum))
        (fxmod
          (fxand
            (force-exact (hash-proc obj (byte-length obj) (if initr (initr) 0)))
            most-positive-fixnum)
          bound) ) ) ) )

;;; HASH function signatures

(define (make-bounded-hash hash-proc . args)
  (let-optionals args ((byte-length ##sys#size) (initr #f))
    (check-procedure 'make-bounded-hash hash-proc)
    (check-procedure 'make-bounded-hash byte-length)
    (when initr (check-procedure 'make-bounded-hash initr))
    (lambda (obj . args)
      (let-optionals args ((bound most-positive-fixnum))
        (abs (modulo (hash-proc obj (byte-length obj) (if initr (initr) 0)) bound)) ) ) ) )

(define (make-seeded-hash hash-proc . args)
	(check-procedure 'make-seeded-hash hash-proc)
	(let-optionals args ((seed (current-hash-seed)))
		(check-unsigned-integer32 'make-seeded-hash seed)
		(lambda (str . args)
			(hash-proc str (optional args (##sys#size str)) seed)) ) )

(define (make-range-hash hash-proc upper . args)
	(check-procedure 'make-range-hash hash-proc)
	(let-optionals args ((lower 0))
		(check-unsigned-integer32 'make-range-hash lower)
		(check-unsigned-integer32 'make-range-hash upper)
		(unless (<= lower upper) (swap-set! upper lower))
		(if (zero? lower) (lambda (str . args) (modulo (apply hash-proc str args) upper))
      (let ((diff (- upper lower -1)))
        (lambda (str . args) (+ lower (modulo (apply hash-proc str args) diff))) ) ) ) )

(define (make-mask-hash hash-proc mask)
	(check-procedure 'make-mask-hash hash-proc)
  (check-unsigned-integer32 'make-mask-hash mask)
	(lambda (str . args)
		(bitwise-and (apply hash-proc str args) mask)) )

(define (make-real-hash hash-proc)
	(check-procedure 'make-real-hash hash-proc)
	(lambda (str . args)
		(let ((h (apply hash-proc str args)))
			(if (zero? h) 0.0 (/ 1.0 h)) ) ) )

;;;

;; Takes 2 arguments - 1 required & 1 optional:
;; hash-primitive-procedure
;; length-procedure
;;
;; Returns 1 value:
;; hash-procedure

(define (make-hash-procedure prim-proc #!optional (byte-length ##sys#size))
  (check-procedure 'make-hash-procedure prim-proc)
  (lambda (data . args)
    (let-optionals args ((length (byte-length data)) (initval 0))
      (prim-proc data length initval)) ) )

;; Takes 1 argument:
;; hash-primitive-procedure
;;
;; Returns 1 value:
;; message-digest-primitive

(define (make-hash-message-digest-primitive prim-proc #!optional (name #f))
  (check-procedure 'make-hash-message-digest-procedures prim-proc)
  (let ((updt-proc (%make-hash-update-procedure prim-proc))
        (name (or (and name
                       (string->uninterned-symbol
                         (string-append
                          (cond
                            ((symbol? name) (##sys#symbol->string name))
                            ((string? name) name)
                            (else
                              (error-argument-type 'make-hash-message-digest-primitive
                                name "symbol or string" 'name)))
                          "-primitive")))
                  (gensym "hash-primitive-"))) )
    (make-message-digest-primitive
      hash-context-size
      unsigned-integer32-size
      generic-init
      updt-proc
      generic-final
      name) ) )

) ;module hash-utils
