;;;; CRCHash.scm
;;;; Kon Lovett, Jan '06

;; Issues
;;
;; - CRC hash accumulation technique is questionable.

(module CRCHash

  (;export
    *CRCHash
    CRCHash
    CRCHash-primitive)

  (import scheme chicken foreign)

  (use message-digest box crc hash-utils)

  (include "hashes-macros")

;;;

(define (*CRCHash str len hash)
	(let ((crc (crc32 str len)))
		(if hash (bitwise-xor crc hash)
			crc ) ) )

(define (CRCHash str . args)
  (let-optionals args ((len (##sys#size str)) (seed #f))
    (*CRCHash str len seed) ) )

;;;

(define (CRCHash-context)
	(make-box) )

(define (CRCHash-init ctx)
	(box-set! ctx #f) )

(define (CRCHash-update ctx str len)
	(box-set! ctx (*CRCHash str len (box-ref ctx))) )

(define (CRCHash-final ctx result)
	(unsigned-integer32-set! result (box-ref ctx)) )

;;;

(define CRCHash-primitive
  (let ((the-prim #f))
    (lambda ()
      (or the-prim
          (begin
            (set! the-prim
                  (make-message-digest-primitive
                    CRCHash-context
                    unsigned-integer32-size
                    CRCHash-init CRCHash-update CRCHash-final))
            the-prim ) ) ) ) )

) ;module CRCHash
